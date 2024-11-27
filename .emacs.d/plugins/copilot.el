;; https://github.com/copilot-emacs/copilot.el?tab=readme-ov-file

(req_package 'editorconfig)

(add-to-list 'load-path "~/.emacs.d/lisp/copilot.el")

(require 'copilot)

(add-hook 'prog-mode-hook 'copilot-mode)

(add-to-list 'copilot-major-mode-alist '("enh-go" . "go"))

(add-to-list 'copilot-indentation-alist '(prog-mode 2))

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
