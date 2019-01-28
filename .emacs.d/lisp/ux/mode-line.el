;;; mode-line.el --- Mode line configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--modeline-config ()
  "Initialize Emacs mode line."
  (validate-setq
   ;; Show row and column numbers.
   line-number-mode t
   column-number-mode t)

  ;;; (Packages) ;;;
  ;; Display the current time and system load.
  (use-package time
    :config
    (validate-setq
     display-time-24hr-format t
     display-time-day-and-date t)
    (display-time))

  ;; Display battery status.
  (use-package battery
    :config
    (when (and (functionp battery-status-function)
               (not (string-match-p "N/A" (battery-format "%B" (funcall battery-status-function)))))
      (validate-setq battery-mode-line-format
                     (format " [%s%s%s]" "%b%p%" " (%t)"
                             (if (string-match-p "N/A" (battery-format "%d" (funcall battery-status-function)))
                                 ""
                               " %d°C")))
      (display-battery-mode t)))

  (use-package doom-modeline
    :hook (after-init-hook . doom-modeline-init)
    :config
    (validate-setq
     doom-modeline-height 18))

  (use-package mode-line-bell
    :config
    (mode-line-bell-mode t)))

(user--modeline-config)


(provide 'ux/mode-line)
;;; mode-line.el ends here
