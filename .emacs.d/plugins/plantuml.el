(req_package 'plantuml-mode)

;; C-c C-c  plantuml-preview: renders a PlantUML diagram from the current buffer in the best supported format
;; C-u C-c C-c  plantuml-preview in other window
;; C-u C-u C-c C-c plantuml-preview in other frame

(setq plantuml-executable-path "/usr/bin/plantuml")
(setq plantuml-default-exec-mode 'executable)

;; Enable plantuml-mode for PlantUML files
;(add-to-list 'auto-mode-alist '(("\\.plantuml\\'" . plantuml-mode)
;                                ("\\.puml\\'" . plantuml-mode)))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

(setq plantuml-output-type "png")
