// Required dependencies for TUI and Markdown rendering.
use crate::crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::{Backend, CrosstermBackend},
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span, Text, ToText},
    widgets::{Block, Borders, List, ListItem, ListState, Paragraph, Widget},
    Frame, Terminal,
};
use std::{
    env, fs, io,
    path::{Path, PathBuf},
    time::Duration,
};

// FEATURE: Import termimad for professional Markdown rendering (Requirement 2).
use termimad::*;
// FEATURE: Enum to track the currently focused panel for input handling.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Panel {
    FileTree,
    Editor,
}

// FEATURE: Struct to hold the state of the simple text editor (Requirement 3).
struct EditorState {
    content: Vec<String>,
    cursor_x: usize,
    cursor_y: usize,
}

impl EditorState {
    fn new() -> Self {
        Self {
            content: vec![String::new()],
            cursor_x: 0,
            cursor_y: 0,
        }
    }

    fn reset_content(&mut self, content: Vec<String>) {
        self.content = if content.is_empty() {
            vec![String::new()]
        } else {
            content
        };
        self.cursor_x = 0;
        self.cursor_y = 0;
    }

    // Helper to ensure the cursor is within bounds for the current line
    fn clamp_cursor_x(&mut self) {
        let line_len = self.content[self.cursor_y].len();
        if self.cursor_x > line_len {
            self.cursor_x = line_len;
        }
    }
}

// FEATURE: TreeEntry remains the same for the file explorer.
struct TreeEntry {
    entry: fs::DirEntry,
    depth: usize,
}

// FEATURE: App structure modified to support new features (Requirements 1, 2, 3, 4).
struct App {
    should_quit: bool,
    recursive_view: bool,
    // FEATURE: Base path to restrict navigation (Requirement 4).
    base_path: PathBuf,
    current_path: PathBuf,
    current_entries: Vec<TreeEntry>,
    current_selected: ListState,

    // FEATURE: Content panels.
    active_panel: Panel,
    viewer_markdown_content: String,
    editor_state: EditorState,
}

impl App {
    fn new() -> Self {
        // FEATURE: Initialize base_path and current_path to 'content/' (Requirement 4).
        let base_path = env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("/"))
        .join("content");

        // Ensure the initial path exists, otherwise default to current_dir
        let current_path = if base_path.exists() && base_path.is_dir() {
            base_path.clone()
        } else {
            // If 'content' doesn't exist, create it or use a fallback.
            // For a robust app, we'll try to create it.
            if let Err(e) = fs::create_dir_all(&base_path) {
                eprintln!("Could not create content directory: {}", e);
            }
            base_path.clone()
        };


        let mut app = Self {
            should_quit: false,
            recursive_view: false,
            // FEATURE: New field initializations.
            base_path,
            current_path,
            current_entries: Vec::new(),
            current_selected: ListState::default(),

            active_panel: Panel::FileTree,
            viewer_markdown_content: String::new(),
            editor_state: EditorState::new(),
        };

        app.current_selected.select(Some(0));
        app.update_panels();
        // FEATURE: Load initial content for the panels
        app.update_content_panels();
        app
    }

    // FEATURE: Removed redundant 'parent_entries' and 'parent_selected' logic.

    // ... (read_dir_entries and build_recursive_tree remain the same) ...
    fn read_dir_entries(path: &Path) -> io::Result<Vec<fs::DirEntry>> {
        let mut entries = fs::read_dir(path)?
        .filter_map(|res| res.ok())
        .collect::<Vec<_>>();

        entries.sort_by_key(|entry| {
            let is_dir = entry.file_type().map(|ft| ft.is_dir()).unwrap_or(false);
            // Sort directories first, then files
            (!is_dir, entry.file_name())
        });
        Ok(entries)
    }

    fn build_recursive_tree(
        path: &Path,
        current_depth: usize,
        entries_list: &mut Vec<TreeEntry>,
    ) -> io::Result<()> {
        let entries = Self::read_dir_entries(path)?;

        for entry in entries {
            let path = entry.path();
            let is_dir = entry.file_type().map(|ft| ft.is_dir()).unwrap_or(false);

            entries_list.push(TreeEntry {
                entry,
                depth: current_depth,
            });

            if is_dir {
                // Ignore errors during recursion but continue
                let _ = Self::build_recursive_tree(&path, current_depth + 1, entries_list);
            }
        }
        Ok(())
    }
    // ... (end of read_dir_entries and build_recursive_tree) ...

    fn update_panels(&mut self) {
        self.current_entries.clear();

        if self.recursive_view {
            // Ignore error here, will be handled by UI showing empty list
            let _ = Self::build_recursive_tree(&self.current_path, 0, &mut self.current_entries);
        } else {
            if let Ok(entries) = Self::read_dir_entries(&self.current_path) {
                for entry in entries {
                    self.current_entries.push(TreeEntry { entry, depth: 0 });
                }
            }
        }

        if self.current_entries.is_empty() {
            self.current_selected.select(None);
        } else {
            // Select 0, or clamp to new max
            let new_max = self.current_entries.len() - 1;
            if let Some(selected) = self.current_selected.selected() {
                if selected > new_max {
                    self.current_selected.select(Some(new_max));
                }
            } else {
                self.current_selected.select(Some(0));
            }
        }
        // FEATURE: Removed parent_entries and parent_selected updates.
    }

    fn toggle_recursive_view(&mut self) {
        self.recursive_view = !self.recursive_view;
        self.update_panels();
        // FEATURE: Update content panels after changing view, as selection might change
        self.update_content_panels();
    }

    fn toggle_fold(&mut self) {
        // ... (toggle_fold logic remains the same) ...
        let selected_idx = match self.current_selected.selected() {
            Some(i) => i,
            None => return,
        };

        let (current_path, current_depth) = {
            let selected_item = &self.current_entries[selected_idx];

            if !selected_item
                .entry
                .file_type()
                .map(|ft| ft.is_dir())
                .unwrap_or(false)
                {
                    return;
                }
                (selected_item.entry.path(), selected_item.depth)
        };

        let is_unfolded = self
        .current_entries
        .get(selected_idx + 1)
        .map_or(false, |next_item| next_item.depth > current_depth);

        if is_unfolded {
            let end_range = self
            .current_entries
            .iter()
            .skip(selected_idx + 1)
            .position(|item| item.depth <= current_depth)
            .map_or(self.current_entries.len(), |i| i + selected_idx + 1);

            if end_range > selected_idx + 1 {
                self.current_entries.drain(selected_idx + 1..end_range);
            }
        } else {
            let mut new_entries = Vec::new();
            // Ignoring errors here for robust UI
            let _ = Self::build_recursive_tree(&current_path, current_depth + 1, &mut new_entries);

            if !new_entries.is_empty() {
                self.current_entries
                .splice(selected_idx + 1..selected_idx + 1, new_entries);
            }
        }
        // ... (end of toggle_fold logic) ...
    }

    fn enter_directory(&mut self) {
        if let Some(selected_idx) = self.current_selected.selected() {
            if let Some(tree_entry) = self.current_entries.get(selected_idx) {
                if tree_entry
                    .entry
                    .file_type()
                    .map(|ft| ft.is_dir())
                    .unwrap_or(false)
                    {
                        self.current_path = tree_entry.entry.path();
                        self.recursive_view = false;
                        self.update_panels();
                        // FEATURE: Update content after navigation
                        self.update_content_panels();
                    } else {
                        // FEATURE: If a file is selected, update content panels (Requirements 2 & 3)
                        self.update_content_panels();
                    }
            }
        }
    }

    fn leave_directory(&mut self) {
        // FEATURE: Restrict navigation above the base_path (Requirement 4)
        if self.current_path != self.base_path {
            if self.current_path.pop() {
                self.recursive_view = false;
                self.update_panels();
                // FEATURE: Update content after navigation
                self.update_content_panels();
            }
        }
    }

    fn select_next(&mut self) {
        let i = match self.current_selected.selected() {
            Some(i) => {
                if i >= self.current_entries.len().saturating_sub(1) {
                    0
                } else {
                    i + 1
                }
            }
            None => 0,
        };
        self.current_selected.select(Some(i));
        // FEATURE: Update content panels after selection change
        self.update_content_panels();
    }

    fn select_previous(&mut self) {
        let i = match self.current_selected.selected() {
            Some(i) => {
                if i == 0 {
                    self.current_entries.len().saturating_sub(1)
                } else {
                    i - 1
                }
            }
            None => self.current_entries.len().saturating_sub(1),
        };
        self.current_selected.select(Some(i));
        // FEATURE: Update content panels after selection change
        self.update_content_panels();
    }

    fn get_selected_entry(&self) -> Option<&fs::DirEntry> {
        self.current_selected
        .selected()
        .and_then(|i| self.current_entries.get(i))
        .map(|tree_entry| &tree_entry.entry)
    }

    // FEATURE: Helper to construct the interactive file path (Requirement 3).
    fn get_interactive_path(file_path: &Path) -> PathBuf {
        if let Some(os_name) = file_path.file_stem() {
            if let Some(ext) = file_path.extension() {
                let mut name = os_name.to_os_string();
                name.push("-interactive."); // The key part
                name.push(ext);
                return file_path.with_file_name(name);
            }
        }
        file_path.to_path_buf()
    }

    // FEATURE: Method to update Markdown content for viewer and editor (Requirements 2 & 3).
    fn update_content_panels(&mut self) {
        self.viewer_markdown_content.clear();
        self.editor_state.reset_content(vec![String::new()]);

        if let Some(selected_entry) = self.get_selected_entry() {
            let path = selected_entry.path();
            if path.is_file() && path.extension().map_or(false, |ext| ext == "md") {
                // Read-only Markdown file (Viewer)
                match fs::read_to_string(&path) {
                    Ok(content) => self.viewer_markdown_content = content,
                    Err(e) => {
                        self.viewer_markdown_content = format!("Error reading file: {}\nPath: {}", e, path.display());
                    }
                }

                // Editable Interactive file (Editor)
                let interactive_path = Self::get_interactive_path(&path);
                match fs::read_to_string(&interactive_path) {
                    Ok(content) => {
                        let lines: Vec<String> = content.lines().map(|s| s.to_string()).collect();
                        self.editor_state.reset_content(lines);
                    }
                    Err(_) => {
                        // If the interactive file doesn't exist, start with empty content
                        // and show a note in the viewer.
                        self.editor_state.reset_content(vec![String::new()]);
                    }
                }
            }
        }
    }

    // FEATURE: Implement basic text editor logic (Requirement 3).
    fn handle_editor_input(&mut self, key_code: KeyCode, modifiers: KeyModifiers) {
        let state = &mut self.editor_state;
        let y = state.cursor_y;
        let line_count = state.content.len();

        // Handle save operation (Ctrl+S)
        if modifiers.contains(KeyModifiers::CONTROL) && key_code == KeyCode::Char('s') {
            if let Err(e) = self.save_editor_content() {
                eprintln!("Error saving file: {}", e);
            }
            return;
        }

        // Handle navigation and edits
        match key_code {
            KeyCode::Char(c) => {
                state.content[y].insert(state.cursor_x, c);
                state.cursor_x += 1;
            }
            KeyCode::Enter => {
                let current_line = &mut state.content[y];
                let new_line = current_line.split_off(state.cursor_x);
                state.content.insert(y + 1, new_line);
                state.cursor_y += 1;
                state.cursor_x = 0;
            }
            KeyCode::Backspace => {
                if state.cursor_x > 0 {
                    state.cursor_x -= 1;
                    state.content[y].remove(state.cursor_x);
                } else if state.cursor_y > 0 {
                    // Merge line up
                    let prev_line = state.content.remove(y);
                    state.cursor_y -= 1;
                    let prev_len = state.content[state.cursor_y].len();
                    state.content[state.cursor_y].push_str(&prev_line);
                    state.cursor_x = prev_len;
                }
            }
            KeyCode::Delete => {
                if state.cursor_x < state.content[y].len() {
                    state.content[y].remove(state.cursor_x);
                } else if state.cursor_y < line_count - 1 {
                    // Merge line down
                    let next_line = state.content.remove(y + 1);
                    state.content[y].push_str(&next_line);
                }
            }
            KeyCode::Left => {
                if state.cursor_x > 0 {
                    state.cursor_x -= 1;
                }
            }
            KeyCode::Right => {
                if state.cursor_x < state.content[y].len() {
                    state.cursor_x += 1;
                }
            }
            KeyCode::Up => {
                if state.cursor_y > 0 {
                    state.cursor_y -= 1;
                    state.clamp_cursor_x();
                }
            }
            KeyCode::Down => {
                if state.cursor_y < line_count - 1 {
                    state.cursor_y += 1;
                    state.clamp_cursor_x();
                }
            }
            _ => {}
        }
    }

    // FEATURE: Method to save the content of the editor panel (Requirement 3).
    fn save_editor_content(&self) -> io::Result<()> {
        if let Some(selected_entry) = self.get_selected_entry() {
            let viewer_path = selected_entry.path();
            if viewer_path.is_file() && viewer_path.extension().map_or(false, |ext| ext == "md") {
                let interactive_path = Self::get_interactive_path(&viewer_path);
                // Join lines back with newline characters for saving
                let content = self.editor_state.content.join("\n");
                fs::write(interactive_path, content)?;
            }
        }
        Ok(())
    }
}

fn main() -> Result<(), io::Error> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let mut app = App::new();
    let res = run_app(&mut terminal, &mut app);

    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
             LeaveAlternateScreen,
             DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        eprintln!("{err:?}");
    }

    Ok(())
}

fn run_app<B: Backend>(terminal: &mut Terminal<B>, app: &mut App) -> io::Result<()> {
    loop {
        // FEATURE: Only show cursor if the Editor panel is active
        if app.active_panel == Panel::Editor {
            // NOTE: Cursor handling is done in the ui function based on content.
        } else {
            terminal.hide_cursor()?;
        }

        terminal.draw(|f| ui::<B>(f, app))?;

        if event::poll(Duration::from_millis(250))? {
            if let Event::Key(key) = event::read()? {
                // Global key handling
                match key.code {
                    KeyCode::Char('q') | KeyCode::Esc => app.should_quit = true,
                    // FEATURE: Use Tab to switch between FileTree and Editor
                    KeyCode::Tab => {
                        app.active_panel = match app.active_panel {
                            Panel::FileTree => Panel::Editor,
                            Panel::Editor => Panel::FileTree,
                        };
                    }
                    _ => {
                        // Contextual key handling based on active panel
                        match app.active_panel {
                            Panel::FileTree => match key.code {
                                KeyCode::Char('e') => app.toggle_recursive_view(),
                                KeyCode::Char('t') => app.toggle_fold(),
                                KeyCode::Char('j') | KeyCode::Down => app.select_next(),
                                KeyCode::Char('k') | KeyCode::Up => app.select_previous(),
                                KeyCode::Char('h') | KeyCode::Backspace | KeyCode::Left => {
                                    app.leave_directory()
                                }
                                KeyCode::Char('l') | KeyCode::Enter | KeyCode::Right => {
                                    app.enter_directory()
                                }
                                _ => {}
                            },
                            Panel::Editor => {
                                // FEATURE: Delegate input to the editor state handler
                                app.handle_editor_input(key.code, key.modifiers);
                            }
                        }
                    }
                }
            }
        }

        if app.should_quit {
            return Ok(());
        }
    }
}

// FEATURE: Custom widget for rendering the editable text area with a cursor.
struct EditorWidget<'a> {
    state: &'a EditorState,
    is_active: bool,
}

impl<'a> Widget for EditorWidget<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let lines: Vec<Line> = self
        .state
        .content
        .iter()
        .enumerate()
        .map(|(i, s)| {
            // If this is the cursor line
            if i == self.state.cursor_y && self.is_active {
                let (pre, post) = s.split_at(self.state.cursor_x);
                let (cursor_char, post_rest) = if post.is_empty() {
                    // At the end of the line, use a block cursor
                    (" ", "")
                } else {
                    // Normal cursor over a character
                    post.split_at(1)
                };

                let spans = vec![
                    Span::raw(pre),
             // Cursor character with inverted style
             Span::styled(
                 cursor_char,
                 Style::default().add_modifier(Modifier::REVERSED),
             ),
             Span::raw(post_rest),
                ];
                Line::from(spans)
            } else {
                Line::from(s.to_string())
            }
        })
        .collect();

        // Render the content, respecting the area's height (to avoid rendering lines out of bounds)
        let paragraph = Paragraph::new(Text::from(lines));
        paragraph.render(area, buf);
    }
}

fn ui<B: Backend>(f: &mut Frame, app: &mut App) {
    // Top-level vertical split: Header (1 line) and Content (rest)
    let main_chunks = Layout::default()
    .direction(Direction::Vertical)
    .constraints([Constraint::Length(1), Constraint::Min(0)])
    .split(f.size());

    let header_chunk = main_chunks[0];
    let content_chunk = main_chunks[1];

    // Header displaying the current path and status
    let status_str = format!(
        "Path: {} | Active Panel: {}",
        app.current_path.to_string_lossy(),
                             if app.active_panel == Panel::FileTree {
                                 "File Tree (TAB to Editor)"
                             } else {
                                 "Editor (TAB to Tree, CTRL+S to Save)"
                             }
    );
    let header = Paragraph::new(status_str)
    .style(Style::default().bg(Color::Blue).fg(Color::White));
    f.render_widget(header, header_chunk);

    // FEATURE: Three-panel horizontal split for content (Requirement 1).
    let content_chunks = Layout::default()
    .direction(Direction::Horizontal)
    .constraints([
        Constraint::Percentage(20), // File Tree
                 Constraint::Percentage(55), // Markdown Viewer
                 Constraint::Percentage(25), // Markdown Editor
    ])
    .split(content_chunk);

    // --- Panel 1: File Tree (List) ---
    let title = if app.recursive_view {
        "File Tree (R: Recursive 'e')"
    } else {
        "File Tree (F: Flat 'e')"
    };
    let current_items: Vec<ListItem> = app
    .current_entries
    .iter()
    .map(|tree_entry| format_entry_tree(tree_entry))
    .collect();

    let file_tree_block = Block::default()
    .borders(Borders::ALL)
    .title(title)
    .border_style(if app.active_panel == Panel::FileTree {
        Style::default().fg(Color::LightYellow) // Highlight active panel
    } else {
        Style::default().fg(Color::DarkGray)
    });

    let current_list = List::new(current_items)
    .block(file_tree_block)
    .highlight_style(
        Style::default()
        .bg(Color::LightBlue)
        .fg(Color::Black)
        .add_modifier(Modifier::BOLD),
    );
    f.render_stateful_widget(current_list, content_chunks[0], &mut app.current_selected);

    // --- Panel 2: Markdown Viewer (Read-Only) (Requirement 2) ---
    let viewer_block = Block::default()
    .borders(Borders::ALL)
    .title("Markdown Viewer (Read-Only)");

    // FEATURE: Use termimad to render the markdown content professionally.
    // FIX: Correct termimad rendering logic.
    let markdown_text = app.viewer_markdown_content.clone();
    let skin = MadSkin::default();

    // 2. Convert the Compound structure into a ratatui::Text object, respecting the available width.

    // THE NEW FIX: Explicitly get the FmtText struct, then explicitly call the
    // .to_text() conversion method which returns the correct Result type,
    // thereby isolating the unwrap_or_else call from the ambiguous FmtText.
    let fmt_text = skin.text(&markdown_text, Some(content_chunks[1].width.into()));

    let rich_text = fmt_text.to_text();

    let viewer_paragraph = Paragraph::new(rich_text).block(viewer_block).wrap(ratatui::widgets::Wrap { trim: false });
    f.render_widget(viewer_paragraph, content_chunks[1]);


    // This achieves the same result: if the Markdown rendering succeeds, we get the `rich_text`; if it fails, we execute the closure to display the error message instead. It's concise and safe!

    // --- Panel 3: Markdown Editor (Editable) (Requirement 3) ---
    let editor_block = Block::default()
    .borders(Borders::ALL)
    .title("Interactive Markdown Editor (Tab to activate)");

    // Use the custom EditorWidget to render the editable content.
    let editor_widget = EditorWidget {
        state: &app.editor_state,
        is_active: app.active_panel == Panel::Editor,
    };
    f.render_widget(editor_widget, content_chunks[2]);

    // FEATURE: Set the TUI cursor position if the Editor is active.
    if app.active_panel == Panel::Editor {
        let editor_area = content_chunks[2];
        let state = &app.editor_state;

        // Calculate the actual cursor position relative to the terminal window
        // (x + 1 for block border, y + 1 for block border)
        let cursor_x = editor_area.x + 1 + state.cursor_x as u16;
        let cursor_y = editor_area.y + 1 + state.cursor_y as u16;

        f.set_cursor(cursor_x, cursor_y);
    }
}

// ... (format_entry_tree, format_entry_flat, get_entry_info, format_size remain the same) ...

fn format_entry_flat(entry: &fs::DirEntry) -> ListItem {
    let file_name = entry.file_name().to_string_lossy().to_string();
    let metadata = entry.metadata().ok();
    let is_dir = metadata.as_ref().map(|m| m.is_dir()).unwrap_or(false);

    let (icon, style) = if is_dir {
        ("📁 ", Style::default().fg(Color::Cyan))
    } else {
        ("📄 ", Style::default().fg(Color::White))
    };

    ListItem::new(format!("{icon}{file_name}")).style(style)
}

fn format_entry_tree(tree_entry: &TreeEntry) -> ListItem {
    let entry = &tree_entry.entry;
    let file_name = entry.file_name().to_string_lossy().to_string();
    let metadata = entry.metadata().ok();
    let is_dir = metadata.as_ref().map(|m| m.is_dir()).unwrap_or(false);

    let (icon, style) = if is_dir {
        ("📁 ", Style::default().fg(Color::Cyan)) // Directory
    } else {
        ("📄 ", Style::default().fg(Color::White)) // File
    };

    let indent = "  ".repeat(tree_entry.depth);

    ListItem::new(format!("{indent}{icon}{file_name}")).style(style)
}

fn get_entry_info(entry: &fs::DirEntry) -> String {
    let mut info = String::new();
    info.push_str(&format!("Name: {}\n", entry.file_name().to_string_lossy()));

    if let Ok(metadata) = entry.metadata() {
        let file_type = if metadata.is_dir() {
            "Directory"
        } else if metadata.is_file() {
            "File"
        } else if metadata.is_symlink() {
            "Symlink"
        } else {
            "Other"
        };
        info.push_str(&format!("Type: {}\n", file_type));

        if metadata.is_file() {
            info.push_str(&format!("Size: {}\n", format_size(metadata.len())));
        }

        if let Ok(modified) = metadata.modified() {
            // Using a simple epoch seconds display for TUI simplicity
            if let Ok(duration) = modified.duration_since(std::time::SystemTime::UNIX_EPOCH) {
                info.push_str(&format!("Modified (epoch): {}\n", duration.as_secs()));
            }
        }

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let perms = metadata.permissions();
            info.push_str(&format!("Perms: {:o}\n", perms.mode() & 0o777));
        }
    } else {
        info.push_str("Could not read metadata.\n");
    }

    info
}

fn format_size(bytes: u64) -> String {
    const KB: u64 = 1024;
    const MB: u64 = KB * 1024;
    const GB: u64 = MB * 1024;

    if bytes >= GB {
        format!("{:.2} GB", bytes as f64 / GB as f64)
    } else if bytes >= MB {
        format!("{:.2} MB", bytes as f64 / MB as f64)
    } else if bytes >= KB {
        format!("{:.2} KB", bytes as f64 / KB as f64)
    } else {
        format!("{} B", bytes)
    }
}
