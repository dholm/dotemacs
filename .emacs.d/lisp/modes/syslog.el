;;; syslog --- a mode for syslogs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--syslog-mode-hook ()
  "Hook for syslog-mode."
  ;; There is no need to spell check log files.
  (flyspell-mode-off)

  ;;; (Bindings) ;;;
  (local-set-key (kbd "f") 'syslog-filter-lines)
  (local-set-key (kbd "F") 'hide-lines-show-all))

(use-package syslog-mode
  :defer
  :mode "\(/var/log.*\|\.log\)$"
  :init
  (add-hook 'syslog-mode-hook 'user--syslog-mode-hook))


(provide 'modes/syslog)
;;; syslog.el ends here
