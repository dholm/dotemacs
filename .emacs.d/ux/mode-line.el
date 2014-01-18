;;; mode-line.el --- Mode line configuration
;;; Commentary:
;;; Code:

(defun user/smart-mode-line-init ()
  "Initialize smart mode line."
  (setq-default
   ;; Configure theme
   sml/theme 'dark
   ;; Shorten path and modes
   sml/shorten-directory t
   sml/shorten-mode t
   sml/name-width 25
   sml/mode-width 'full)

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(sml/global ((t (,@fmt-none
                           :foreground ,s-mode-line-fg
                           :box (:line-width 1 :color ,s-mode-line-bg :style unspecified)))))
         '(sml/modes ((t (:inherit sml/global :foreground ,solarized-comment))))

         '(sml/line-number ((t (:inherit sml/modes ,@fmt-bold))))
         '(sml/position-percentage ((t (:inherit sml/modes))))
         '(sml/col-number ((t (:inherit sml/modes))))
         '(sml/numbers-separator ((t (:inherit sml/modes))))

         '(sml/mule-info ((t (:inherit sml/modes))))
         '(sml/client ((t (:inherit sml/modes))))

         '(sml/prefix ((t (:inherit sml/modes))))
         '(sml/folder ((t (:inherit sml/modes))))
         '(sml/filename ((t (:inherit sml/global :foreground ,solarized-emph ,@fmt-bold))))

         '(sml/not-modified ((t (:inherit sml/modes))))
         '(sml/modified ((t (:inherit sml/global :foreground ,yellow))))
         '(sml/outside-modified ((t (:inherit sml/modified :foreground ,orange))))
         '(sml/read-only ((t (:inherit sml/modified :foreground ,red))))

         '(sml/vc ((t (:inherit sml/global :foreground ,yellow))))
         '(sml/vc-edited ((t (:inherit sml/vc :foreground ,orange))))
         '(sml/git ((t (:inherit sml/vc))))

         '(sml/charging ((t (:inherit sml/global :foreground ,green))))
         '(sml/discharging ((t (:inherit sml/global :foreground ,red))))

         '(sml/time ((t (:inherit sml/modes)))))))

    (setq-default
     sml/active-foreground-color (face-attribute 'mode-line :foreground)
     sml/active-background-color (face-attribute 'mode-line :background)
     sml/inactive-foreground-color (face-attribute 'mode-line-inactive :foreground)
     sml/inactive-background-color (face-attribute 'mode-line-inactive :background))

    (sml/setup)))


(defun user/modeline-init ()
  "Initialize Emacs modeline."
  (setq-default
   ;; Show row and column numbers.
   line-number-mode t
   column-number-mode t)

  ;; Display the current time and system load.
  (require 'time)
  (setq-default
   display-time-24hr-format t
   display-time-form-list (list 'time 'load)
   display-time-day-and-date t)
  (display-time)

  ;; Display battery charge, if available.
  (when (and (require 'battery nil t)
           (functionp battery-status-function)
           (not (string-match-p "N/A" (battery-format "%B" (funcall battery-status-function)))))
    (setq-default battery-mode-line-format
                  (format " [%s%s%s]" "%b%p%" " (%t)"
                          (if (string-match-p "N/A" (battery-format "%d" (funcall battery-status-function)))
                              ""
                            " %d°C")))
    (display-battery-mode t))

  ;;; (Packages) ;;;
  (require-package '(:name diminish))
  (require-package '(:name smart-mode-line :after (user/smart-mode-line-init))))

(user/modeline-init)


(provide 'ux/mode-line)
;;; mode-line.el ends here
