;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala


;; highlight FIXME, TODO, etc
(req_package 'fic-mode)
;; ide-like plugin for scala
(req_package 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook '(lambda () (fic-mode 1)))
