;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ui

;; completion UI: vertico + orderless + marginalia + consult
;;
;; vertico     — vertical completion list in minibuffer
;; orderless   — match space-separated words in any order
;; marginalia  — annotations next to candidates (file size, docstring, etc.)
;; consult     — enhanced commands with live preview:
;;   C-x b     — consult-buffer (buffers + recent files + bookmarks)
;;   C-s       — consult-line (search current buffer with live preview)
;;   M-g g     — consult-goto-line
;;   M-s r     — consult-ripgrep (project-wide search, needs ripgrep)

(req_package 'vertico)
(vertico-mode 1)

;; Backspace deletes path component instead of single char
(require 'vertico-directory)
(define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
(define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)

(req_package 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(req_package 'marginalia)
(marginalia-mode 1)

(req_package 'consult)
(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "C-s") #'consult-line)
(global-set-key (kbd "M-g g") #'consult-goto-line)
(global-set-key (kbd "M-s r") #'consult-ripgrep)

;; vertico/consult/orderless faces — tango-dark palette
(custom-set-faces
 ;; current candidate
 '(vertico-current ((t (:background "#555753" :foreground "#eeeeec"))))
 ;; orderless match highlights
 '(orderless-match-face-0 ((t (:foreground "#8ae234" :weight bold))))
 '(orderless-match-face-1 ((t (:foreground "#729fcf" :weight bold))))
 '(orderless-match-face-2 ((t (:foreground "#fcaf3e" :weight bold))))
 '(orderless-match-face-3 ((t (:foreground "#e090d7" :weight bold))))
 ;; marginalia annotations
 '(marginalia-documentation ((t (:foreground "#888a85"))))
 '(marginalia-file-priv-dir ((t (:foreground "#729fcf"))))
 '(marginalia-size ((t (:foreground "#888a85"))))
 '(marginalia-date ((t (:foreground "#888a85"))))
 ;; consult preview line
 '(consult-preview-line ((t (:background "#3e4446"))))
 '(consult-preview-match ((t (:background "#ce5c00" :foreground "#eeeeec")))))

;; no startup msg  
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
;; Make *scratch* buffer blank.
(setq initial-scratch-message nil)


;; font size for notebook
(req_package 'better-defaults)
;; (set-face-attribute 'default nil :family "Inconsolata" :height 130 :weight 'bold)
;; (set-face-attribute 'default nil :family "Inconsolata" :height 100 :weight 'bold)
;; (set-face-attribute 'default nil :family "iosevka-ss01" :height 100 :weight 'bold)
(add-to-list 'default-frame-alist '(font . "Source Code Pro-10:bold"))

;; scroll for one string
(setq scroll-step 1)

;; center cursor after PgUp/PgDn and jumps
(setq scroll-preserve-screen-position t)
(advice-add 'scroll-up-command :after (lambda (&rest _) (recenter)))
(advice-add 'scroll-down-command :after (lambda (&rest _) (recenter)))

;; current line highlighting
(global-hl-line-mode 1)
;; global-hl-line-highlight sits on the global post-command-hook and causes
;; flickering in vterm on every keystroke; skip it entirely for vterm buffers
(define-advice global-hl-line-highlight (:before-while () skip-vterm)
  "Don't run hl-line highlight in vterm buffers."
  (not (derived-mode-p 'vterm-mode)))

;; save session
;; (desktop-save-mode t)

;; 'y' or 'n' instead of 'yes' and 'no' in dialogs
(fset 'yes-or-no-p 'y-or-n-p)

;; line numbers
;(global-linum-mode 1)
(global-display-line-numbers-mode)

;; set position to up left corner
(setq initial-frame-alist '((top . 0) (left . 0) ))

;; resize
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        (add-to-list 'default-frame-alist
                     (cons 'width (/ (x-display-pixel-width) (frame-char-width))))
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 0) (frame-char-height)))))))
(set-frame-size-according-to-resolution)


;; color scheme
(req_package 'color-theme-modern)
(setq color-theme-load-all-themes nil)
(load-theme 'tango-dark t) ;; t - for no-confirm
;; (require 'tango-dark-theme)

;; set the current line highlight color
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)

;; set color for lsp highlight
(set-face-attribute 'lsp-face-highlight-textual nil
		    :background "#666" :foreground "#ffffff"
                   )


;; window moving by alt+arrows
;(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; turn on blinking cursor (0 = blink forever)
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)

;; claude-code.el does (setq-local blink-cursor-mode nil) which kills
;; global blinking; undo that after its vterm setup runs
(with-eval-after-load 'claude-code
  (advice-add 'claude-code--term-configure :after
              (lambda (&rest _)
                (kill-local-variable 'blink-cursor-mode))))

;; ;; set default browser
;; (setq browse-url-browser-function 'browse-url-chromium)

;; reload buffers on change silently
(setq auto-revert-verbose nil)
(global-auto-revert-mode t)

;; no lock files (.#filename) — they break file watchers, linters, build tools
(setq create-lockfiles nil)

;; keep backup~ and #auto-save# files out of project directories
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;; expand window
(add-hook 'window-setup-hook #'toggle-frame-maximized)

;; disable warning buffer automatic opening
(setq warning-minimum-level :error)
(add-to-list 'display-buffer-alist
             '("\\*Warnings\\*"
               (display-buffer-no-window)))


;; company tooltip colors
(set-face-attribute 'company-preview nil :foreground "darkgray" :underline t)
(set-face-attribute 'company-preview-common nil :inherit 'company-preview)
(set-face-attribute 'company-tooltip nil :background "lightgray" :foreground "black")
(set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip :weight 'bold)
(set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection :weight 'bold)
(set-face-attribute 'company-tooltip-selection nil :background "steelblue" :foreground "white")

(provide 'ui-loader)
