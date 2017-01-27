;;; alert.el --- Emacs notifications.
;;; Commentary:
;;; Code:

(defun user/alert-style ()
  "Get the preferred alert style."
  (cond
   ((eq system-type 'darwin) 'growl)
   ((executable-find "notify-send") 'libnotify)
   ((executable-find "dbus-send") 'notifications)
   (t 'mode-line)))


(defun user--alert-config ()
  "Initialize alert."
  (setq-default
   ;; Send alerts to alert buffer.
   alert-default-style 'log
   ;; Disable log.
   alert-log-messages nil))

(req-package alert
  :config (user--alert-config))


(provide 'utilities/alert)
;;; alert.el ends here
