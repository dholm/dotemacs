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

  (use-package smart-mode-line
    :config
    (validate-setq
     ;; Configure theme.
     sml/theme 'automatic
     ;; Shorten path and modes.
     sml/shorten-directory t
     sml/shorten-modes t
     sml/name-width 25
     sml/mode-width 'full)

    (with-eval-after-load 'solarized-theme
      (sml/setup)
      (unless (featurep 'projectile)
        (setq
         ;; Temporary workaround for sml thinking projectile has been loaded.
         sml/projectile-loaded-p nil)))))

(user--modeline-config)


(provide 'ux/mode-line)
;;; mode-line.el ends here
