;; https://github.com/stevemolitor/claude-code.el
;;
;; Keybindings (prefix C-c C):
;;   C-c C c   — start Claude session
;;   C-c C m   — command menu (transient)
;;   C-c C t   — toggle Claude window
;;   C-c C s   — send command via minibuffer
;;   C-c C x   — send command with file/line context
;;   C-c C r   — send region or buffer
;;   C-c C o   — send current file
;;   C-c C e   — fix error at point (flycheck/flymake)
;;   C-c C k   — kill session
;;   C-c C y/n — send yes/no
;;   C-c C M   — cycle mode (default/auto-accept/plan)

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

;; C-c c занят под comment toggle, используем C-c C
(global-set-key (kbd "C-c C") claude-code-command-map)

;; Linux notifications
(defun my-claude-notify (title message)
  (if (executable-find "notify-send")
      (call-process "notify-send" nil nil nil title message)
    (message "%s: %s" title message)))
(setq claude-code-notification-function #'my-claude-notify)

(provide 'claude-loader)
