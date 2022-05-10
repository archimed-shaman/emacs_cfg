;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ui

;; no startup msg  
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
;; Make *scratch* buffer blank.
(setq initial-scratch-message nil)


;; font size for notebook
(req_package 'better-defaults)
;(set-face-attribute 'default nil :family "Inconsolata" :height 130 :weight 'bold)
;; (set-face-attribute 'default nil :family "Inconsolata" :height 100 :weight 'bold)
(set-face-attribute 'default nil :family "iosevka-ss01" :height 100 :weight 'bold)

;; scroll for one string
(setq scroll-step 1)

;; scroll for one string
(setq scroll-step 1)

;; current line higliting
(global-hl-line-mode 1)

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

;; turn on blinking cursor
 (blink-cursor-mode 1)

(provide 'ui-loader)

;; set default browser
(setq browse-url-browser-function 'browse-url-chromium)

;; reload buffers on change
(global-auto-revert-mode t)
