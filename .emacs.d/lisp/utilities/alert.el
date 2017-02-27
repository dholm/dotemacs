;;; alert.el --- Emacs notifications. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/alert-style ()
  "Get the preferred alert style."
  (cond
   ((eq system-type 'darwin) 'growl)
   ((executable-find "notify-send") 'libnotify)
   ((executable-find "dbus-send") 'notifications)
   (t 'mode-line)))

(use-package alert
  :defer
  :config
  ;; Undiagnosed issue with validate-setq.
  (setq
   ;; Send alerts to alert buffer.
   alert-default-style 'log)
  (validate-setq
   ;; Disable log.
   alert-log-messages nil))


(provide 'utilities/alert)
;;; alert.el ends here
