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

(defun my-plantuml-auto-preview ()
  (when (eq major-mode 'plantuml-mode)
    (plantuml-preview 0)))

(add-hook 'after-save-hook 'my-plantuml-auto-preview)
(add-hook 'find-file-hook 'my-plantuml-auto-preview)


(setq plantuml-output-type "png")
