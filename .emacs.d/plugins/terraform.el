;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; terraform
;;
;; LSP server: terraform-ls (hashicorp)
;; Linter:     tflint (provider-aware checks)
;; Install:
;;   pacman -S terraform tflint
;;   yay -S terraform-ls-bin

(req_package 'terraform-mode)
(req_package 'lsp-mode)
(req_package 'flycheck)

;; LSP — validation, completion, hover, go-to-definition
(when (executable-find "terraform-ls")
  (add-hook 'terraform-mode-hook #'lsp-deferred))

;; tflint — provider-aware linting via flycheck
(when (executable-find "tflint")
  (flycheck-define-checker terraform-tflint
    "Terraform linter using tflint."
    :command ("tflint" "--format=compact" source-original)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (message (one-or-more (not (any "\n")))) " (WARNING)" line-end)
     (error line-start (file-name) ":" line ":" column ": "
            (message (one-or-more (not (any "\n")))) " (ERROR)" line-end)
     (info line-start (file-name) ":" line ":" column ": "
           (message (one-or-more (not (any "\n")))) " (NOTICE)" line-end))
    :modes terraform-mode)
  (add-to-list 'flycheck-checkers 'terraform-tflint)
  (add-hook 'terraform-mode-hook #'flycheck-mode))

;; Format on save using `terraform fmt`
(when (executable-find "terraform")
  (add-hook 'terraform-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'terraform-format-buffer nil t))))

(provide 'terraform)
;;; terraform.el ends here
