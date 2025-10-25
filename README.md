# fit
(noun) : something clever never  

A terminal-based merge conflict resolver with a three-pane TUI interface, written in Modern Fortran.

## Features

- **Three-Pane Layout**: Visual side-by-side comparison similar to VS Code's merge conflict UI
  - Left pane: Incoming changes (from remote/HEAD)
  - Right pane: Local changes (current branch)
  - Bottom pane: Live preview of your resolution choice

- **Color-Coded Diffs**:
  - Green `+` for incoming additions
  - Red `-` for local changes
  - Clear visual distinction between versions

- **Sequential Navigation**: Navigate through multiple conflicts with keyboard shortcuts

- **Interactive Resolution**: Choose incoming, local, or both changes for each conflict

## Installation

### Prerequisites
- [Fortran Package Manager (fpm)](https://fpm.fortran-lang.org/)
- A modern Fortran compiler (gfortran, ifort, etc.)

### Build
```bash
fpm build
```

### Install
```bash
fpm install
```

## Usage

```bash
fit <file-with-conflicts>
```

Example:
```bash
fit myfile.txt
```

## Keyboard Controls

| Key | Action |
|-----|--------|
| `i` | Select **incoming** changes (from remote/HEAD) |
| `l` | Select **local** changes (current branch) |
| `b` | Select **both** changes (incoming then local) |
| `n` / `→` / `↓` | Navigate to **next** conflict |
| `p` / `←` / `↑` | Navigate to **previous** conflict |
| `s` | **Save** resolved file and quit |
| `q` / `ESC` | **Quit** without saving |
