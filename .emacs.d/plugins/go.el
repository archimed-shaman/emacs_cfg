;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go

(req_package 'golint)
(req_package 'use-package)
(req_package 'fic-mode)
(req_package 'company)
(req_package 'flycheck)
(req_package 'multi-compile)
(req_package 'go-eldoc)
(req_package 'company-go)
(req_package 'yasnippet)

(add-to-list 'yas-snippet-dirs "/home/archimed/dev/emacs_cfg/.emacs.d/yasnippet-golang/")
(yas-reload-all)
(set 'yas-global-mode 1)

;; do:
;; go get -u github.com/mdempsky/gocode

(setenv "GOPATH" (concat (getenv "HOME") "/go"))
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
(setq company-idle-delay .2)
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
(setq multi-compile-alist '(
    (go-mode . (
("go-build" "go build -v"
   (locate-dominating-file buffer-file-name ".git"))
("go-build-and-run" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
   (multi-compile-locate-file-dir ".git"))))
    ))


(add-hook 'go-mode-hook '(lambda () (fic-mode 1)))

;(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))


(provide 'go-loader)
