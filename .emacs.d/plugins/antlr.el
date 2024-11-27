(req_package 'antlr-mode)

(use-package antlr-mode
  :commands (antlr-mode)
  :mode (
         ("\\.g4\\'" . antlr-mode))
 )

(provide 'antlr)
