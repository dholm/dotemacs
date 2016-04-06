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


(defun user/alert-init ()
  "Initialize alert."
  (setq-default
   ;; Send alerts to alert buffer.
   alert-default-style 'log
   ;; Disable log.
   alert-log-messages nil))

(require-package '(:name alert :after (user/alert-init)))


(provide 'utilities/alert)
;;; alert.el ends here
