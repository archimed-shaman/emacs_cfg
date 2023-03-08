;;
;; To install language server:
;; M-x lsp-install-server RET ts-ls RET
;; M-x lsp-install-server RET css-ls RET

(req_package 'lsp-mode)
(req_package 'yasnippet)
(req_package 'lsp-treemacs)
;; (req_package 'helm-lsp)
(req_package 'projectile)
;; (req_package 'hydra)
(req_package 'flycheck)
(req_package 'company)
(req_package 'avy)
;; (req_package 'which-key)
;; (req_package 'helm-xref)
(req_package 'dap-mode)
(req_package 'json-mode)
(req_package 'typescript-mode)


(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in js files
  :hook (js-mode . lsp))


(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in css files
  :hook (css-mode . lsp))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in ts files
  :hook (typescript-mode . lsp))


(use-package lsp-mode
  :mode ("\\.\\(tsx\\)$" . typescript-mode))


;; (setq js-indent-level 2)


(defun my-setup-indent ()
  (setq-local js-indent-level 2)
  (setq-local css-indent-offset 2)
  (setq tab-width 2))

(add-hook 'js-mode-hook 'my-setup-indent)
(add-hook 'css-mode-hook 'my-setup-indent)
(add-hook 'typescript-mode-hook 'my-setup-indent)


(add-hook 'js-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'lsp-format-buffer)
            ))

(add-hook 'css-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'lsp-format-buffer)
            ))


(add-hook 'typescript-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'lsp-format-buffer)
            ))


;; (helm-mode)
;; (require 'helm-xref)
;; (define-key global-map [remap find-file] #'helm-find-files)
;; (define-key global-map [remap execute-extended-command] #'helm-M-x)
;; (define-key global-map [remap switch-to-buffer] #'helm-mini)
;; (which-key-mode)
;; (add-hook 'prog-mode-hook #'lsp)
;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       company-idle-delay 0.0
;;       company-minimum-prefix-length 1
;;       create-lockfiles nil) ;; lock files will kill `npm start'

;; (with-eval-after-load 'lsp-mode
;;   (require 'dap-chrome)
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;   (yas-global-mode))

(provide 'js-loader)

