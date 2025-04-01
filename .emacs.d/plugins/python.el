;; Before:
;; pip install -U python-language-server autopep8 pydocstyle pyflakes --break-system-packages
;; M-x treesit-install-language-grammar

;; (use-package lsp-mode
;;   :hook (python-mode . lsp))

;; (setq major-mode-remap-alist
;;       '((python-mode . python-ts-mode)))

;; (defun python-imenu-use-flat-index
;;     ()
;;   (setq imenu-create-index-function
;;         #'python-imenu-create-flat-index))

;; (add-hook 'python-mode-hook
;;           #'python-imenu-use-flat-index)


;; pip install -U "python-lsp-server[all]" --break-system-packages
;; pip install -U pylsp-mypy python-lsp-isort python-lsp-black pyls-memestra pylsp-rope python-lsp-ruff pyflakes flake8 pycodestyle --break-system-packages

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

;(use-package lsp-mode
;  :hook (python-mode . lsp))

;; (defun python-imenu-use-flat-index
;;     ()
;;   (setq imenu-create-index-function
;;         #'python-imenu-create-flat-index))

;; (add-hook 'python-mode-hook
;;           #'python-imenu-use-flat-index)


(provide 'python)
