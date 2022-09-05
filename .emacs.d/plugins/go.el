;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go

(req_package 'go-mode)
(req_package 'flycheck)
(req_package 'lsp-mode)
(req_package 'auto-complete)
(req_package 'go-complete)
(req_package 'use-package)


;; do:
;; go get -u github.com/nsf/gocode
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u github.com/josharian/impl
;; go get -u golang.org/x/tools/cmd/godoc
;; GO111MODULE=on go get golang.org/x/tools/gopls@latest
;; go get golang.org/x/tools/gopls@latest
;; GO111MODULE=on go get mvdan.cc/gofumpt
;; GO111MODULE=on go get mvdan.cc/gofumpt/gofumports
;; go get -u github.com/dougm/goflymake

(add-hook 'go-mode-hook 'lsp-deferred)


;; On-fly error check
;(require 'go-flymake)
;(require 'go-flycheck)


;(ac-config-default)
;(global-auto-complete-mode t)
;; Autocomplete
(require 'go-autocomplete)
;;(require 'auto-complete-config)
;(auto-complete-mode 1)

;(add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-golang/")
;(yas-reload-all)
;(set 'yas-global-mode 1)


(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setenv "GOPRIVATE" "github.com/archimed-shaman")
(setq exec-path (append exec-path '(concat (getenv "GOPATH") "/bin")))
(setq exec-path (append exec-path '("~/go/bin")))


;; auto complete brackets
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'before-save-hook 'lsp-format-buffer)
(add-hook 'before-save-hook 'lsp-organize-imports)
(setq-default gofmt-command "gofumpt")


(add-hook 'go-mode-hook '(lambda () (fic-mode 1)))

(add-hook 'go-mode-hook '(lambda() (setq indent-tabs-mode t)))



(provide 'go-loader)
