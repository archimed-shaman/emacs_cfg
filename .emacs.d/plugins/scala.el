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
(req_package 'scala-mode)
(req_package 'sbt-mode)
;; ide-like plugin for scala
(req_package 'ensime)
(req_package 'use-package)


(use-package ensime
  :ensure t
  :pin melpa-stable)
(set 'ensime-startup-notification nil)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))


;; For complex scala files
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 5000)

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
                            ))

(provide 'scala-loader)
