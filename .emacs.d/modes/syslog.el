;;; syslog --- a mode for syslogs
;;; Commentary:
;;; Code:

(defun dholm/syslog-mode-init ()
  "Initialize syslog-mode."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(syslog-ip-face ((t (:foreground ,yellow :background unspecified))))
         '(syslog-hour-face ((t (:foreground ,green :background unspecified))))
         '(syslog-error-face ((t (:foreground ,red :background unspecified ,@fmt-bold))))
         '(syslog-warn-face ((t (:foreground ,orange :background unspecified ,@fmt-bold))))
         '(syslog-info-face ((t (:foreground ,blue :background unspecified ,@fmt-bold))))
         '(syslog-debug-face ((t (:foreground ,cyan :background unspecified ,@fmt-bold))))
         '(syslog-su-face ((t (:foreground ,magenta :background unspecified))))))))

  ;; Register auto mode
  (add-auto-mode 'syslog-mode "/var/log.*$"))

(require-package '(:name syslog-mode
			 :depends (hide-lines)
			 :type github
			 :pkgname "vapniks/syslog-mode"
			 :after (dholm/syslog-mode-init)))


(provide 'modes/syslog)
;;; syslog.el ends here
