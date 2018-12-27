(req_package 'irony)
(req_package 'company-irony)
(req_package 'company-irony-c-headers)
(req_package 'company-c-headers)

(add-to-list 'company-backends 'company-c-headers)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'global-company-mode)



;; rub now:
;; irony-install-server <RET>
