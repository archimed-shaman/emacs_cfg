;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reverse-im — keybindings work regardless of keyboard layout
;;
;; With russian-computer input method active, C-ц acts as C-c,
;; C-ч as C-x, M-ы as M-s, etc.

(req_package 'reverse-im)
(reverse-im-activate "russian-computer")

(provide 'reverse-im-loader)
