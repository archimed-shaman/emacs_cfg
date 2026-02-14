;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git
;;
;; magit:
;;   C-x g       — magit-status
;;
;; diff-hl (fringe indicators for uncommitted changes):
;;   Updates live (flydiff), no need to save.
;;   Refreshes automatically after magit operations.
;;
;;   Built-in bindings (via vc prefix C-x v):
;;     C-x v [   — diff-hl-previous-hunk
;;     C-x v ]   — diff-hl-next-hunk
;;     C-x v =   — diff-hl-diff-goto-hunk (show hunk diff)
;;     C-x v n   — diff-hl-revert-hunk

(req_package 'magit)
(req_package 'diff-hl)

(global-diff-hl-mode)
(diff-hl-flydiff-mode)

;; refresh after magit operations
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; colors
(custom-set-faces
 '(diff-hl-insert ((t (:foreground "green" :background "green"))))
 '(diff-hl-delete ((t (:foreground "red" :background "red"))))
 '(diff-hl-change ((t (:foreground "purple" :background "purple")))))


(provide 'git-loader)
