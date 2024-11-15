;; Install:
;; yay -S sqls pgformatter

(setq sql-product 'postgres)


(use-package sqlformat
  :ensure t
  :hook (sql-mode . sqlformat-on-save-mode)
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g" "-B" "-f2" "-U2" "-u2")))

(use-package lsp-mode
  :ensure t
  :hook (sql-mode . lsp)
  :config
  (setq lsp-sql-format "pg_format -s2 -g -B -f2 -U2 -u2"))


(add-hook 'sql-mode-hook
          (lambda ()
            (setq-local tab-width 2)
            (setq-local indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist
             '("\\.sql$" . (lambda ()
                             (sql-mode)
                             (sql-highlight-postgres-keywords))))

(provide 'sql)
;;; sql.el ends here
