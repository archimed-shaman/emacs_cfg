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

(provide 'visual-loader)
