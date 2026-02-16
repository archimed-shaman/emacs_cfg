;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visual effects: pulsar, highlight-indent-guides, rainbow-delimiters

;; pulsar — pulse current line on navigation events
(req_package 'pulsar)
(pulsar-global-mode 1)

;; highlight-indent-guides — thin vertical indentation lines in code buffers
(req_package 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)

;; rainbow-delimiters — color brackets/parens by nesting depth
(req_package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; doom-modeline — informative modeline with icons
(req_package 'nerd-icons)
(req_package 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-icon t)
(setq doom-modeline-minor-modes t)

;; minions — hide minor modes behind a menu
(req_package 'minions)
(minions-mode 1)

;; modeline colors for tango-dark:
;; dark active (readable with doom-modeline's colored segments),
;; grey inactive (not a black hole)
(custom-set-faces
 '(mode-line          ((t (:background "#2e3436" :foreground "#eeeeec" :box (:line-width 1 :color "#555753")))))
 '(mode-line-inactive ((t (:background "#555753" :foreground "#babdb6" :box (:line-width 1 :color "#555753"))))))

(provide 'visual-loader)
