;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python
;;
;; LSP server: pylsp (python-lsp-server)
;; Install: yay -S python-lsp-server

(add-hook 'python-mode-hook #'lsp-deferred)


(provide 'python)
