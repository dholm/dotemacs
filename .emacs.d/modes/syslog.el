;;; syslog --- a mode for syslogs
;;; Commentary:
;;; Code:

(defun user/syslog-mode-hook ()
  "Hook for syslog-mode."
  ;; There is no need to spell check log files.
  (flyspell-mode-off))


(defun user/syslog-mode-init ()
  "Initialize syslog-mode."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(syslog-ip-face ((t (:foreground ,yellow :background unspecified))))
         '(syslog-hour-face ((t (:foreground ,green :background unspecified))))
         '(syslog-error-face ((t (:foreground ,red :background unspecified
                                              :weight bold))))
         '(syslog-warn-face ((t (:foreground ,orange :background unspecified
                                             :weight bold))))
         '(syslog-info-face ((t (:foreground ,blue :background unspecified
                                             :weight bold))))
         '(syslog-debug-face ((t (:foreground ,cyan :background unspecified
                                              :weight bold))))
         '(syslog-su-face ((t (:foreground ,magenta :background unspecified))))))))

  ;; Register auto mode
  (add-auto-mode 'syslog-mode "/var/log.*$")

  (add-hook 'syslog-mode-hook 'user/syslog-mode-hook))


(require-package '(:name hide-lines))
(require-package '(:name syslog-mode :after (user/syslog-mode-init)))


(provide 'modes/syslog)
;;; syslog.el ends here
