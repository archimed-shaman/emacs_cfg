(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide package loader
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/.emacs.d/lisp"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; refresh package list if it is not already available
(when (not package-archive-contents) (package-refresh-contents))

(defun req_package (package)
  (unless (package-installed-p package)
    (package-install package))
    (require 'package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load plugins
(load-directory "~/.emacs.d/plugins")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ui
;; font size for notebook
(req_package 'better-defaults)
(set-face-attribute 'default nil :height 80)

;; scroll for one string
(setq scroll-step 1)

;; scroll for one string
(setq scroll-step 1)

;; current line higliting
(global-hl-line-mode 1)

;; save session
;; (desktop-save-mode t)

;; 'y' or 'n' instead of 'yes' and 'no' in dialogs
(fset 'yes-or-no-p 'y-or-n-p)

;; line numbers
(global-linum-mode 1)

;; set position to up left corner
(setq initial-frame-alist '((top . 0) (left . 0) ))

;; resize
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        (add-to-list 'default-frame-alist
                     (cons 'width (/ (x-display-pixel-width) (frame-char-width))))
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 0) (frame-char-height)))))))
(set-frame-size-according-to-resolution)


;; color scheme
(req_package 'color-theme)
(setq color-theme-load-all-themes nil)
(require 'tango-dark-theme)

;; set the current line highlight color
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)


;; window moving by alt+arrows
;(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight FIXME, TODO, etc
(req_package 'fic-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala

(req_package 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook '(lambda () (fic-mode 1)))
