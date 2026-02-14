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

;; VC-installed packages: pin to "manual" so package-upgrade-all doesn't
;; replace them with same-named MELPA packages (e.g. yuya373/claude-code-emacs).
;; Update these via M-x package-vc-upgrade instead.
(dolist (pkg '(inheritenv claude-code monet))
  (add-to-list 'package-pinned-packages (cons pkg "manual")))

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
          (when window
            (select-window window)
            ;; side window creation doesn't trigger window-size-change-functions,
            ;; so vterm never gets the resize signal; force it
            (run-at-time 0.2 nil #'window--adjust-process-windows))
          window)))

;; vterm handles window resize better than eat in vertical splits
(req_package 'vterm)
(setq claude-code-terminal-backend 'vterm)

;; let terminal handle resize natively, the optimization breaks vertical splits
(setq claude-code-optimize-window-resize nil)

;; default 0.1s is too short — Claude renders before vterm resize arrives
(setq claude-code-startup-delay 0.5)

;; line numbers make no sense in a terminal and mess up vterm width calculation
(add-hook 'claude-code-start-hook
          (lambda () (display-line-numbers-mode -1)))

;; C-c c is taken by comment toggle, use C-c C instead
(global-set-key (kbd "C-c C") claude-code-command-map)

;; emacsclient needs a running server for CLI hooks
(require 'server)
(unless (server-running-p) (server-start))

;; --- CLI hooks: auto-open edited files in Emacs ---
;; Ensure ~/.claude/settings.json has PostToolUse hook that calls emacsclient.
;; The hook checks CLAUDE_BUFFER_NAME (set only by Emacs) so console sessions
;; are unaffected.  Runs once at config load; safe to re-run (idempotent).
(defun my-claude-ensure-cli-hooks ()
  "Add PostToolUse hook to ~/.claude/settings.json if missing."
  (let* ((settings-file (expand-file-name "~/.claude/settings.json"))
         (settings (if (file-exists-p settings-file)
                       (with-temp-buffer
                         (insert-file-contents settings-file)
                         (json-read))
                     '()))
         (hook-cmd (concat "bash -c '"
                           "test -n \"$CLAUDE_BUFFER_NAME\" && "
                           "emacsclient --eval "
                           "\"(claude-code-handle-hook (quote post-tool-use) "
                           "\\\"$CLAUDE_BUFFER_NAME\\\")\" "
                           "\"$(cat)\" 2>/dev/null || true"
                           "'"))
         (hook-entry `((matcher . "")
                       (hooks . [((type . "command")
                                  (command . ,hook-cmd))])))
         (hooks (alist-get 'hooks settings))
         (post-hooks (alist-get 'PostToolUse hooks)))
    ;; only add if no PostToolUse hooks exist yet
    (unless post-hooks
      (let* ((new-post `(PostToolUse . [,hook-entry]))
             (new-hooks (if hooks
                            (cons new-post hooks)
                          (list new-post)))
             (new-settings (cons (cons 'hooks new-hooks)
                                 (assq-delete-all 'hooks settings))))
        (make-directory (file-name-directory settings-file) t)
        (with-temp-file settings-file
          (insert (json-encode new-settings))
          (json-pretty-print-buffer))))))
(my-claude-ensure-cli-hooks)

;; When Claude edits a file, open it in the main (non-Claude) window
(defun my-claude-open-edited-file (message)
  "Open files modified by Claude in the main editing window.
Reverts the buffer to pick up changes, then scrolls to the first edit."
  (when (eq (plist-get message :type) 'post-tool-use)
    (condition-case nil
        (let* ((json-data (plist-get message :json-data))
               (parsed (when (and json-data (stringp json-data))
                         (json-read-from-string json-data)))
               (tool-name (alist-get 'tool_name parsed))
               (tool-input (alist-get 'tool_input parsed))
               (file-path (or (alist-get 'file_path tool-input)
                              (alist-get 'notebook_path tool-input))))
          (when (and file-path
                     (member tool-name '("Edit" "Write" "MultiEdit"))
                     (file-exists-p file-path))
            (let* ((buf (find-file-noselect file-path))
                   ;; extract search target: new_string from Edit, first edit from MultiEdit
                   (search-str
                    (cond
                     ((string= tool-name "Edit")
                      (alist-get 'new_string tool-input))
                     ((string= tool-name "MultiEdit")
                      (let ((edits (alist-get 'edits tool-input)))
                        (when (and edits (> (length edits) 0))
                          (alist-get 'new_string (aref edits 0)))))
                     (t nil)))
                   ;; use just the first line of the new text for search
                   (search-line
                    (when (and search-str (not (string-empty-p search-str)))
                      (car (split-string search-str "\n" t)))))
              ;; revert buffer to pick up Claude's changes
              (with-current-buffer buf
                (revert-buffer t t t))
              ;; show in a non-side-window, don't steal focus
              (let ((win (display-buffer buf '((display-buffer-use-some-window)
                                               (inhibit-same-window . t)))))
                ;; scroll to first change
                (when (and search-line win)
                  (with-selected-window win
                    (goto-char (point-min))
                    (when (search-forward search-line nil t)
                      (beginning-of-line)
                      (recenter 3))))))))
      (error nil))))
(add-hook 'claude-code-event-hook #'my-claude-open-edited-file)

;; Linux notifications
(defun my-claude-notify (title message)
  (if (executable-find "notify-send")
      (call-process "notify-send" nil nil nil title message)
    (message "%s: %s" title message)))
(setq claude-code-notification-function #'my-claude-notify)

(provide 'claude-loader)
