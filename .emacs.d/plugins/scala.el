;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala

;; 1) you have to install sbt by hand
;; 2) make something like this:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; // ensime-sbt is needed for the integration tests
;; addSbtPlugin("org.ensime" % "ensime-sbt" % "0.4.0")
;; 
;; // BUG https://github.com/sbt/sbt-header/issues/31
;; //addSbtPlugin("de.heikoseeberger" % "sbt-header" % "1.5.0")
;; 
;; // not working on Windows https://github.com/sbt/sbt/issues/1952
;; //addMavenResolverPlugin
;; 
;; addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.5.1")
;; 
;; // waiting for 1.3.6 https://github.com/scoverage/sbt-scoverage/issues/153
;; // addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "1.3.5")
;; // sbt-coveralls needs a new release
;; // https://github.com/scoverage/sbt-coveralls/issues/52
;; //addSbtPlugin("org.scoverage" %% "sbt-coveralls" % "1.0.1")
;; 
;; addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.1")
;; 
;; scalacOptions in Compile ++= Seq("-feature", "-deprecation")
;; 
;; // sbt, STFU...
;; ivyLoggingLevel := UpdateLogging.Quiet
;; 
;; addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.5.0")
;; 
;; resolvers += Resolver.sonatypeRepo("snapshots")
;; 
;; addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.5-SNAPSHOT")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; highlight FIXME, TODO, etc
(req_package 'fic-mode)
(req_package 'lsp-mode)
(req_package 'lsp-metals)
(req_package 'lsp-ui)
(req_package 'company)
(req_package 'scala-mode)
(req_package 'posframe)
(req_package 'dap-mode)
(req_package 'sbt-mode)
(req_package 'gradle-mode)
(req_package 'groovy-mode) ;; for gradle
(req_package 'use-package)

;; ide-like plugin for scala
;;(req_package 'ensime)


;;(use-package ensime
;;  :ensure t
;;  :pin melpa-stable)
;;(set 'ensime-startup-notification nil)

;;(use-package scala-mode
;;  :interpreter
;;  ("scala" . scala-mode))


;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package scala-mode
  :mode (("\\.s\\(cala\\|bt\\)$" . scala-mode)
         ("\\.\\(gradle\\)$" . scala-mode)
         ))

;; (add-hook 'gradle-mode-hook '(lambda ()
;;                                (scala-mode)
;;                                (groovy-mode)
;;                               ))


(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui)

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;;   to avoid odd behavior with snippets and indentation
(use-package yasnippet)

;; Use company-capf as a completion provider.
;;
;; To Company-lsp users:
;;   Company-lsp is no longer maintained and has been removed from MELPA.
;;   Please migrate to company-capf.
(use-package company
  :hook (scala-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf))

;; For complex scala files
;; (setq max-lisp-eval-depth 50000)
;; (setq max-specpdl-size 5000)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )

;; auto complete brackets
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook '(lambda ()
                              (fic-mode 1)
                              (setq prettify-symbols-alist scala-prettify-symbols-alist)
                              (prettify-symbols-mode)
                              (add-hook 'before-save-hook 'scalafmt-before-save)
                              ))

(defun scalafmt-before-save ()
  (lsp-format-buffer))

(provide 'scala-loader)
