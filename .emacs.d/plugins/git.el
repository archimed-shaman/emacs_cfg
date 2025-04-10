(req_package 'magit)
(req_package 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
;;(git-gutter:linum-setup)

;; If you enable git-gutter-mode for some modes
;(add-hook 'ruby-mode-hook 'git-gutter-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

(custom-set-variables
 '(git-gutter:modified-sign " ") ;; two space
 '(git-gutter:added-sign "+")    ;; multiple character is OK
 '(git-gutter:deleted-sign "-"))

(set-face-background 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

(defun refresh-git-gutter ()
  "Refresh git-gutter after saving."
  (when (bound-and-true-p git-gutter-mode)
    (git-gutter:update-all-windows)))

(add-hook 'after-save-hook 'refresh-git-gutter)


(provide 'git-loader)
