;;; syslog --- a mode for syslogs
;;; Commentary:
;;; Code:

(defun user/syslog-mode-hook ()
  "Hook for syslog-mode."
  ;; There is no need to spell check log files.
  (flyspell-mode-off))


(defun user/syslog-mode-init ()
  "Initialize syslog-mode."
  ;; Register auto mode
  (add-auto-mode 'syslog-mode "/var/log.*$")

  (add-hook 'syslog-mode-hook 'user/syslog-mode-hook))


(require-package '(:name hide-lines))
(require-package '(:name syslog-mode :after (user/syslog-mode-init)))


(provide 'modes/syslog)
;;; syslog.el ends here
