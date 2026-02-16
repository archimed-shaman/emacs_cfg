#!/usr/bin/env bash
# Install system dependencies for this Emacs config on Arch Linux.
# Requires: pacman, yay (for AUR packages), go (for Go tools).
set -euo pipefail

# --- Core (always needed) ---
PACMAN_CORE=(
    git                             # version control, magit
    cmake                           # compiles vterm-module.so
    libvterm                        # terminal emulator lib for emacs-vterm
    libnotify                       # notify-send for desktop notifications
    ripgrep                         # fast search, used by consult-ripgrep
    adobe-source-code-pro-fonts     # default UI font
    ttf-sourcecodepro-nerd          # Nerd Font variant for doom-modeline icons
)

# --- Languages & their tooling ---
PACMAN_GO=(
    go                              # Go compiler
)
PACMAN_PYTHON=(
    python-lsp-server               # pylsp language server
)
PACMAN_JS=(
    nodejs                          # JS/TS runtime, copilot, lsp servers
    npm                             # package manager (ts-ls, css-ls installed via lsp-mode)
)
PACMAN_CPP=(
    clang                           # clangd language server for C/C++
)
PACMAN_SCALA=(
    sbt                             # Scala build tool (metals auto-installed by lsp-metals)
    jdk-openjdk                     # Java SDK for sbt/metals/plantuml
)
PACMAN_TERRAFORM=(
    terraform                       # Terraform CLI (format on save)
    tflint                          # provider-aware Terraform linter
)
AUR_TERRAFORM=(
    terraform-ls-bin                # HashiCorp LSP server (validation, completion, hover)
)
PACMAN_SQL=(
    pgformatter                     # pg_format SQL formatter
)
PACMAN_MISC=(
    w3m                             # terminal browser (eww fallback)
    plantuml                        # UML diagrams
)

# --- AUR packages ---
AUR_PACKAGES=(
    claude-code                     # Claude Code CLI
    multimarkdown                   # markdown processor for markdown-mode
    sqls                            # SQL language server
)

# --- Go tools (installed into $GOPATH/bin) ---
GO_TOOLS=(
    golang.org/x/tools/gopls@latest         # Go language server
    mvdan.cc/gofumpt@latest                 # strict Go formatter
)

# ---------- helpers ----------
info()  { printf '\033[1;34m::\033[0m %s\n' "$*"; }
warn()  { printf '\033[1;33m::\033[0m %s\n' "$*"; }

install_pacman() {
    local -a missing=()
    for pkg in "$@"; do
        pacman -Qi "$pkg" &>/dev/null || missing+=("$pkg")
    done
    if (( ${#missing[@]} )); then
        info "pacman: installing ${missing[*]}"
        sudo pacman -S --needed --noconfirm "${missing[@]}"
    fi
}

install_aur() {
    if ! command -v yay &>/dev/null; then
        warn "yay not found, skipping AUR packages: $*"
        return
    fi
    local -a missing=()
    for pkg in "$@"; do
        pacman -Qi "$pkg" &>/dev/null || missing+=("$pkg")
    done
    if (( ${#missing[@]} )); then
        info "yay: installing ${missing[*]}"
        yay -S --needed --noconfirm "${missing[@]}"
    fi
}

install_go_tools() {
    if ! command -v go &>/dev/null; then
        warn "go not found, skipping Go tools"
        return
    fi
    for tool in "$@"; do
        info "go install $tool"
        go install "$tool"
    done
}

# ---------- main ----------
info "=== Core ==="
install_pacman "${PACMAN_CORE[@]}"

info "=== Go ==="
install_pacman "${PACMAN_GO[@]}"

info "=== Python ==="
install_pacman "${PACMAN_PYTHON[@]}"

info "=== JS/TS ==="
install_pacman "${PACMAN_JS[@]}"

info "=== C/C++ ==="
install_pacman "${PACMAN_CPP[@]}"

info "=== Scala ==="
install_pacman "${PACMAN_SCALA[@]}"

info "=== Terraform ==="
install_pacman "${PACMAN_TERRAFORM[@]}"
install_aur "${AUR_TERRAFORM[@]}"

info "=== SQL ==="
install_pacman "${PACMAN_SQL[@]}"

info "=== Misc ==="
install_pacman "${PACMAN_MISC[@]}"

info "=== AUR ==="
install_aur "${AUR_PACKAGES[@]}"

info "=== Go tools ==="
install_go_tools "${GO_TOOLS[@]}"

info "Done. LSP servers ts-ls and css-ls are installed from Emacs: M-x lsp-install-server"
