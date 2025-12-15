# Greenmerald Notes


> **A powerful, Rust-based Terminal User Interface (TUI) for navigating only content folder, rendering Markdown files efficiently, and opening and editable markdown.**

Greenmerald combines the speed of a CLI with the visual feedback of a GUI. It allows you to explore directories with a tree-view structure, view detailed file information, and render Markdown documents directly in your terminal with full styling support.

---

## ✨ Key Features

- **🚀 Fast Navigation**: Efficiently browse your content folder  using Vim-inspired key bindings.
- **📂 Interactive Tree View**: Toggle between a flat list and a recursive tree structure ('e'). Expand or collapse specific folders ('t') to visualize nested content.
- **📝 Markdown Rendering**: Instantly preview Markdown files with headings, code blocks, lists, and bold/italic styling directly in the preview pane.
- **👀 Triple-Panel View**: View the content directory the mardowns and the interactive markdowns.
- **ℹ️ File Insights**: Get immediate metadata and details for any selected file or directory.
- **📜 Scrollable Previews**: smooth scrolling for long documents and file information.

## 🛠️ Installation

### Prerequisites
- **Rust & Cargo**: Ensure you have a recent version of Rust installed. [Get Rust](https://www.rust-lang.org/tools/install).

### Build from Source

```bash
# Clone the repository
git clone https://github.com/plantacerium/GreenmeraldNotes.git

# Navigate to the project directory
cd greenmerald-tui-markdown-renderer

# Run the application
cargo run
```

Or install it locally:

```bash
cargo install --path .
```

## 🎮 Usage & Controls

Greenmerald uses intuitive keyboard shortcuts for navigation and control.

### Navigation

| Key | Action |
| :--- | :--- |
| `j` / `↓` | Move selection **down** |
| `k` / `↑` | Move selection **up** |
| `l` / `Right` / `Enter` | **Enter** selected directory |
| `h` / `Left` / `Backspace` | **Go back** to parent directory |

### View & Actions

| Key | Action |
| :--- | :--- |
| `e` | **Toggle View Mode**: Switch between Flat View and Recursive Tree View |
| `t` | **Fold/Unfold**: Expand or collapse the selected directory (Tree View only) |
| `Ctrl` + `j` | **Scroll Preview Down**: Move down in the preview/markdown pane |
| `Ctrl` + `k` | **Scroll Preview Up**: Move up in the preview/markdown pane |
| `TAB` | **change pannel** | 
| `CTRL+S` | **Save File** |
| `q` | **Quit** the application |

## 🏗️ Architecture

Greenmerald Notes is built using the **Ratatui** ecosystem:

- **[Ratatui](https://ratatui.rs/)**: The core library for building the TUI.
- **[Crossterm](https://github.com/crossterm-rs/crossterm)**: For terminal manipulation and input handling.
- **[tui-markdown](https://crates.io/crates/tui-markdown)**: For parsing and rendering Markdown content into TUI widgets.

## ⚡ Support

<div align="center">

**Made with ❤️ and ☕ by the Plantacerium**

[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/plantacerium)

⭐ **Star us on GitHub** if the script is useful to You! ⭐

</div>
