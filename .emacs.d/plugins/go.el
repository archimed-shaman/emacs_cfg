;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go

;; (req_package 'golint)
(req_package 'use-package)
(req_package 'lsp-mode)
(req_package 'lsp-ui)
(req_package 'fic-mode)
(req_package 'company)
(req_package 'company-lsp)
;; (req_package 'flycheck)
;; (req_package 'multi-compile)
;; (req_package 'go-eldoc)
;; (req_package 'go-impl)
(req_package 'go-mode)
;; (req_package 'go-rename)
;; (req_package 'company-go)
(req_package 'yasnippet)
;; (req_package 'flycheck-golangci-lint)


(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))


(add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-golang/")
(yas-reload-all)
(set 'yas-global-mode 1)

;; do:
;; go get -u github.com/nsf/gocode
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u github.com/josharian/impl
;; go get -u golang.org/x/tools/cmd/godoc
;; GO111MODULE=on go get golang.org/x/tools/gopls@latest
;; go get golang.org/x/tools/gopls@latest
;; GO111MODULE=on go get mvdan.cc/gofumpt
;; GO111MODULE=on go get mvdan.cc/gofumpt/gofumports


(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setenv "GOPRIVATE" "github.com/archimed-shaman")
(setq exec-path (append exec-path '(concat (getenv "GOPATH") "/bin")))
(setq exec-path (append exec-path '("~/go/bin")))

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

;; auto complete brackets
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

(add-hook 'before-save-hook 'gofmt-before-save)
(setq-default gofmt-command "gofumports")
;; (add-hook 'go-mode-hook 'go-eldoc-setup)


;; (add-hook 'go-mode-hook 'yas-minor-mode)

(add-hook 'go-mode-hook '(lambda () (fic-mode 1)))

(add-hook 'go-mode-hook '(lambda() (setq indent-tabs-mode t)))


(provide 'go-loader)
