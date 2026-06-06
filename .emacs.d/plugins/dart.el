;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dart
;;
;; LSP server: Dart Analysis Server (bundled with the Dart/Flutter SDK)
;; Install:
;;   yay -S dart            ;; or install the Flutter SDK
;; Make sure `dart' (and `flutter' for Flutter projects) is on PATH.

(req_package 'dart-mode)
(req_package 'lsp-dart)
(req_package 'flycheck)
(req_package 'lsp-mode)
(req_package 'lsp-ui)
(req_package 'company)
(req_package 'yasnippet)

;; LSP
(add-hook 'dart-mode-hook #'lsp-deferred)
(add-hook 'dart-mode-hook #'company-mode)
(add-hook 'dart-mode-hook #'yas-minor-mode)

;; Flycheck
(add-hook 'dart-mode-hook #'flycheck-mode)

;; Format on save via LSP (uses `dart format')
(add-hook 'dart-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

;; Highlight FIXME/TODO
(add-hook 'dart-mode-hook (lambda () (fic-mode 1)))

;; Indentation — 2 spaces (Dart style guide), no tabs
(add-hook 'dart-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)
            (setq-local tab-width 2)
            (setq-local dart-format-on-save nil)))

(provide 'dart)
;;; dart.el ends here
