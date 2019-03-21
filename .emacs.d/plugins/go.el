;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go

(req_package 'use-package)
(req_package 'fic-mode)
(req_package 'company)
(req_package 'flycheck)
(req_package 'yasnippet)
(req_package 'multi-compile)
(req_package 'go-eldoc)
(req_package 'company-go)


;; do:
;; go get -u github.com/mdempsky/gocode

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
(add-hook 'go-mode-hook 'yas-minor-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(setq multi-compile-alist '(
    (go-mode . (
("go-build" "go build -v"
   (locate-dominating-file buffer-file-name ".git"))
("go-build-and-run" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
   (multi-compile-locate-file-dir ".git"))))
    ))


(add-hook 'go-mode-hook '(lambda () (fic-mode 1)))

(provide 'go-loader)
