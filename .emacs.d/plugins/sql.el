;;
;; install
;;
;; yay -S sqls pgformatter

(req_package 'sqlformat)

(use-package lsp-mode
  :hook (sql-mode . lsp))

(add-hook 'sql-mode-hook
          (lambda ()
            (setq-default lsp-sql-format "pg_format -s2 -g -B -f2 -U2 -u2")
            ))

(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g" "-B" "-f2" "-U2" "-u2"))


(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)

;; (add-hook 'sql-mode-hook (lambda ()
;;                            (add-hook 'before-save-hook 'sqlformat-buffer)
;;                            ))


(provide 'sql)
;;; sql.el ends here
