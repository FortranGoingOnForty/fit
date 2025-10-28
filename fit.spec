Name:           fit
Version:        0.1.0
Release:        1%{?dist}
Summary:        Terminal-based merge conflict resolver with three-pane TUI interface

License:        MIT
%global debug_package %{nil}
URL:            https://github.com/FortranGoingOnForty/fit
Source0:        %{name}-%{version}.tar.gz

BuildRequires:  gfortran >= 4.8
BuildRequires:  make
Requires:       glibc
Requires:       git

%description
fit is a terminal-based merge conflict resolver with a three-pane TUI interface,
written in Modern Fortran.

Features:
- Three-Pane Layout: Visual side-by-side comparison similar to VS Code's merge conflict UI
  - Left pane: Incoming changes (from remote/HEAD)
  - Right pane: Local changes (current branch)
  - Bottom pane: Live preview of your resolution choice
- Color-Coded Diffs: Green + for incoming additions, Red - for local changes
- Sequential Navigation: Navigate through multiple conflicts with keyboard shortcuts
- Interactive Resolution: Choose incoming, local, or both changes for each conflict

%prep
%autosetup

%build
make release

%install
mkdir -p %{buildroot}%{_bindir}
mkdir -p %{buildroot}%{_docdir}/%{name}

# Install binary
install -Dm755 bin/fit %{buildroot}%{_bindir}/fit

# Install documentation
install -Dm644 README.md %{buildroot}%{_docdir}/%{name}/README.md

%files
%{_bindir}/fit
%{_docdir}/%{name}/README.md

%changelog
* Sun Oct 26 2025 mfw <espadon@outlook.com> - 0.1.0-1
- Initial release of fit
- Terminal-based merge conflict resolver
- Three-pane TUI interface (incoming, local, preview)
- Color-coded diffs with visual indicators
- Interactive resolution with keyboard shortcuts
- Sequential navigation through multiple conflicts
