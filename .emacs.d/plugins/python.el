;; Before:
;; pip install -U python-language-server autopep8 pydocstyle pyflakes--break-system-packages


(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in css files
  :hook (python-mode . lsp))


(defun python-imenu-use-flat-index
    ()
  (setq imenu-create-index-function
        #'python-imenu-create-flat-index))

(add-hook 'python-mode-hook
          #'python-imenu-use-flat-index)

(provide 'python)
