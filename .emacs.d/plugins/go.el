;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go

(req_package 'golint)
(req_package 'use-package)
(req_package 'fic-mode)
(req_package 'company)
(req_package 'flycheck)
(req_package 'multi-compile)
(req_package 'go-eldoc)
(req_package 'go-impl)
(req_package 'go-mode)
(req_package 'go-rename)
(req_package 'company-go)
(req_package 'yasnippet)
(req_package 'flycheck-golangci-lint)

(add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-golang/")
(yas-reload-all)
(set 'yas-global-mode 1)

;; do:
;; go get -u github.com/nsf/gocode
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u github.com/josharian/impl
;; go get -u golang.org/x/tools/cmd/godoc

(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setenv "GOPRIVATE" "github.com/archimed-shaman")
;; (setq exec-path (append exec-path '(concat (getenv "GOPATH") "/bin")))
(setq exec-path (append exec-path '("~/go/bin")))

(use-package go-mode
  :interpreter
  ("go" . go-mode))


;; auto complete brackets
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

(add-hook 'before-save-hook 'gofmt-before-save)
(setq-default gofmt-command "goimports")
(add-hook 'go-mode-hook 'go-eldoc-setup)


(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
(setq company-idle-delay .0)
(custom-set-faces
 '(company-preview
   ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-selection
   ((t (:background "steelblue" :foreground "white"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection)))))

(add-hook 'go-mode-hook 'yas-minor-mode)


(add-hook 'go-mode-hook 'flycheck-mode)
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))
;; (add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup)

(setq multi-compile-alist '(
                            (go-mode . (
                                        ("go-build" "go build -v"
                                         (locate-dominating-file buffer-file-name ".git"))
                                        ("go-build-and-run" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
                                         (multi-compile-locate-file-dir ".git"))))
                            ))


(add-hook 'go-mode-hook '(lambda () (fic-mode 1)))

(add-hook 'go-mode-hook
          '(lambda() 
             (setq indent-tabs-mode t)
             )
          )


(provide 'go-loader)
