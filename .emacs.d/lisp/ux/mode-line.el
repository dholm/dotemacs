;;; mode-line.el --- Mode line configuration
;;; Commentary:
;;; Code:

(defun user/smart-mode-line-init ()
  "Initialize smart mode line."
  (setq-default
   ;; Configure theme.
   sml/theme 'respectful
   ;; Shorten path and modes.
   sml/shorten-directory t
   sml/shorten-mode t
   sml/name-width 25
   sml/mode-width 'full)

  (after-load 'solarized-theme
    (sml/setup)))


(defun user/modeline-init ()
  "Initialize Emacs mode line."
  (setq-default
   ;; Show row and column numbers.
   line-number-mode t
   column-number-mode t)

  ;; Display the current time and system load.
  (with-feature 'time
    (setq-default
     display-time-24hr-format t
     display-time-form-list (list 'time 'load)
     display-time-day-and-date t)
    (display-time))

  ;; Display battery charge, if available.
  (with-feature 'battery
    (when (and (functionp battery-status-function)
               (not (string-match-p "N/A" (battery-format "%B" (funcall battery-status-function)))))
      (setq-default battery-mode-line-format
                    (format " [%s%s%s]" "%b%p%" " (%t)"
                            (if (string-match-p "N/A" (battery-format "%d" (funcall battery-status-function)))
                                ""
                              " %d°C")))
      (display-battery-mode t)))

  ;;; (Packages) ;;;
  (require-package '(:name diminish))
  (require-package '(:name smart-mode-line :after (user/smart-mode-line-init))))

(user/modeline-init)


(provide 'ux/mode-line)
;;; mode-line.el ends here
