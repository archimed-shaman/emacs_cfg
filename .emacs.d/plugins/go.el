;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go

(req_package 'go-mode)
(req_package 'flycheck)
(req_package 'lsp-mode)
(req_package 'auto-complete)
(req_package 'go-complete)
(req_package 'go-autocomplete)
(req_package 'use-package)
(req_package 'go-snippets)


;; do:
;; go install github.com/nsf/gocode@latest
;; go install golang.org/x/tools/cmd/goimports@latest
;; go install github.com/josharian/impl@latest
;; go install golang.org/x/tools/cmd/godoc@latest
;; go install golang.org/x/tools/gopls@latest
;; go install mvdan.cc/gofumpt@latest
;; go install github.com/dougm/goflymake@latest

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

(add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-golang/")
(yas-reload-all)
(yas-global-mode 1)


(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setenv "GOPRIVATE" "github.com/archimed-shaman")
(setq exec-path (append exec-path '(concat (getenv "GOPATH") "/bin")))
(setq exec-path (append exec-path '("~/go/bin")))


;; auto complete brackets
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save nil t)
            (add-hook 'before-save-hook 'lsp-format-buffer nil t)
            (add-hook 'before-save-hook 'lsp-organize-imports nil t)
            (setq-default gofmt-command "gofumpt")
            ))


(add-hook 'go-mode-hook '(lambda () (fic-mode 1)))

(add-hook 'go-mode-hook '(lambda() (setq indent-tabs-mode t)))

(add-hook
 'go-mode-hook
 (lambda ()
   ;; (push '("error" . ?∇) prettify-symbols-alist)
   ;; (push '("err" . ?⊙) prettify-symbols-alist)
   (push '("exists" . ?∃) prettify-symbols-alist)
   (push '(":= range" . ?∈) prettify-symbols-alist)
   ;; (push '("ok" . ?✓) prettify-symbols-alist)
   (push '("==" . ?≡) prettify-symbols-alist)
   (push '(":=" . ?≔) prettify-symbols-alist)
   (push '(">=" . ?≥) prettify-symbols-alist)
   (push '("<=" . ?≤) prettify-symbols-alist)
   (push '("<-" . ?←) prettify-symbols-alist)
   (push '("!=" . ?≠) prettify-symbols-alist)
   (push '("..." . ?…) prettify-symbols-alist)
   (push '("nil" . ?∅) prettify-symbols-alist)
   ;; (push '("make" . ?&) prettify-symbols-alist)
   ;; (push '("new" . ?&) prettify-symbols-alist)
   ;; (push '("context.Context" . ?◇) prettify-symbols-alist)
   ;; (push '("ctx" . ?⋄) prettify-symbols-alist)
   ;; (push '("mu" . ?❢) prettify-symbols-alist)
   (push '("&&" . ?∧) prettify-symbols-alist)
   (push '("||" . ?∨) prettify-symbols-alist)
   (push '("!" . ?¬) prettify-symbols-alist)
   ;; (push '("interface{}" . ?⋆) prettify-symbols-alist)
   ;; (push '("struct{}" . ?ε) prettify-symbols-alist)
   )
 (global-prettify-symbols-mode 't)
 )


(provide 'go)
;;; go.el ends here
