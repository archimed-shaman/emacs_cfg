(req_package 'plantuml-mode)

(setq plantuml-executable-path "/usr/bin/plantuml")
(setq plantuml-default-exec-mode 'executable)

;; Enable plantuml-mode for PlantUML files
;(add-to-list 'auto-mode-alist '(("\\.plantuml\\'" . plantuml-mode)
;                                ("\\.puml\\'" . plantuml-mode)))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

(setq plantuml-output-type "png")
