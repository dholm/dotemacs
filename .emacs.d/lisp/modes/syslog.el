;;; syslog --- a mode for syslogs
;;; Commentary:
;;; Code:

(defun user--syslog-mode-hook ()
  "Hook for syslog-mode."
  ;; There is no need to spell check log files.
  (flyspell-mode-off)

  ;;; (Bindings) ;;;
  (local-set-key (kbd "f") 'syslog-filter-lines)
  (local-set-key (kbd "F") 'hide-lines-show-all))


(defun user--syslog-mode-config ()
  "Initialize syslog-mode."
  ;; Register auto mode for log files.
  (add-auto-mode 'syslog-mode "/var/log.*$" "\\.log$")

  (add-hook 'syslog-mode-hook 'user--syslog-mode-hook))

(req-package syslog-mode
  :config (user--syslog-mode-config))


(provide 'modes/syslog)
;;; syslog.el ends here
