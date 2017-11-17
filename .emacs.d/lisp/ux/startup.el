;;; startup.el --- Configure Emacs startup behavior -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; startup.el has no `provide' statement.
(progn
  (validate-setq
   ;; Do not show the splash screen or message
   inhibit-startup-screen t
   inhibit-startup-echo-area-message (getenv "USER"))

  (use-package dashboard
    :config
    (dashboard-setup-startup-hook)))


(provide 'ux/startup)
;;; startup.el ends here
