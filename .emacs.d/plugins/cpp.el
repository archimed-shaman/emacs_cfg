;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c/c++
;;
;; LSP server: clangd
;; Install: pacman -S clang

(req_package 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

(provide 'cpp-loader)
