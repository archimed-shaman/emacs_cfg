;; https://github.com/stevemolitor/claude-code.el
;; https://github.com/stevemolitor/monet
;;
;; System packages (Arch):
;;   claude-code — CLI itself (AUR: claude-code-cli, or: npm i -g @anthropic-ai/claude-code)
;;   libvterm    — terminal emulator library, needed by emacs-vterm
;;   cmake       — needed to compile vterm-module.so on first load
;;   libnotify   — provides notify-send for desktop notifications
;;
;; Keybindings (prefix C-c C, full list via C-c C m):
;;   C-c C c   — start Claude session
;;   C-c C C   — continue previous conversation
;;   C-c C R   — resume a specific past session
;;   C-c C i   — new named instance
;;   C-c C m   — command menu (transient)
;;   C-c C t   — toggle Claude window
;;   C-c C s   — send command via minibuffer
;;   C-c C x   — send command with file/line context
;;   C-c C r   — send region or buffer
;;   C-c C o   — send current file
;;   C-c C e   — fix error at point (flycheck/flymake)
;;   C-c C f   — fork conversation
;;   C-c C k/K — kill session / kill all
;;   C-c C y   — send <return> (yes/confirm)
;;   C-c C n   — send <escape> (no/cancel)
;;   C-c C z   — toggle read-only mode
;;   C-c C M   — cycle mode (default/auto-accept/plan)
;;   C-c C /   — slash commands menu

;; NonGNU ELPA needed for eat
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(unless (package-installed-p 'eat)
  (package-refresh-contents)
  (package-install 'eat))
(require 'eat)

;; environment inheritance for subprocesses
(unless (package-installed-p 'inheritenv)
  (package-vc-install "https://github.com/purcell/inheritenv"))
(require 'inheritenv)

;; claude-code itself
(unless (package-installed-p 'claude-code)
  (package-vc-install "https://github.com/stevemolitor/claude-code.el"))
(require 'claude-code)

;; IDE integration: open files, show diffs, diagnostics in Emacs
(unless (package-installed-p 'monet)
  (package-vc-install "https://github.com/stevemolitor/monet"))
(require 'monet)
(monet-mode 1)

(claude-code-mode)
(add-hook 'claude-code-process-environment-functions #'monet-start-server-function)

;; split vertically (right side) instead of horizontal (below)
(setq claude-code-display-window-fn
      (lambda (buffer)
        (let ((window (display-buffer buffer
                                      '((display-buffer-in-side-window)
                                        (side . right)
                                        (window-width . 0.4)))))
          (when window (select-window window))
          window)))

;; vterm handles window resize better than eat in vertical splits
(req_package 'vterm)
(setq claude-code-terminal-backend 'vterm)

;; let terminal handle resize natively, the optimization breaks vertical splits
(setq claude-code-optimize-window-resize nil)

;; C-c c is taken by comment toggle, use C-c C instead
(global-set-key (kbd "C-c C") claude-code-command-map)

;; Linux notifications
(defun my-claude-notify (title message)
  (if (executable-find "notify-send")
      (call-process "notify-send" nil nil nil title message)
    (message "%s: %s" title message)))
(setq claude-code-notification-function #'my-claude-notify)

(provide 'claude-loader)
