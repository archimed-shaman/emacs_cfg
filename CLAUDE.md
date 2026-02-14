# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Personal Emacs configuration for a polyglot developer (Go primary, plus Scala, Python, JS/TS, C++, SQL). Uses LSP-based development with AI coding assistants (GitHub Copilot, AgentSmithy/Ollama).

## Architecture

**Entry point**: `.emacs` — loads the package system, sets environment/paths, then recursively loads all `.el` files from `.emacs.d/plugins/`.

**Plugin system**: Each `.emacs.d/plugins/<name>.el` file is a self-contained configuration module for a language or feature. All plugins are loaded automatically via the `load-directory` function — no explicit require/load needed when adding a new plugin file.

**Package management**: Hybrid — `package.el` (ELPA/MELPA) as primary, with `use-package` used in some plugin files (Scala, Python). The helper `req_package` in `.emacs` installs-if-missing then requires a package.

**Key directories**:
- `.emacs.d/plugins/` — modular config files (ui, git, language modes, AI integrations)
- `.emacs.d/lisp/` — external elisp: `copilot.el` (git submodule), `agentsmithy-emacs` (symlink to `~/dev/agentsmithy/agentsmithy-emacs`), custom `tango-dark-theme.el`
- `.emacs.d/elpa/` — installed packages (not committed)
- `.emacs.d/snippets/`, `.emacs.d/yasnippet-golang/` — snippet collections

## Conventions

- Plugin files use `req_package` for dependencies and `provide` at the end
- Format-on-save is configured per language (gofumpt for Go, scalafmt for Scala, lsp-format-buffer for JS/TS)
- Keybinding prefix `C-c` for window movement, `C-c a` for AgentSmithy, `C-c c` for comment toggle
- Go requires external tools installed via `go install` (gopls, goimports, gofumpt, gocode) — see comments in `plugins/go.el`
- The `custom-set-variables`/`custom-set-faces` blocks live in `.emacs` (not in a separate `custom.el` that gets loaded)

## Git Submodules

One submodule: `.emacs.d/lisp/copilot.el` from `https://github.com/copilot-emacs/copilot.el.git`. After cloning, run `git submodule update --init --recursive`.

## Adding a New Language/Feature

Create `.emacs.d/plugins/<name>.el`. It will be auto-loaded. Use `req_package` for dependencies, add LSP hooks if needed, and end with `(provide '<name>)`.
