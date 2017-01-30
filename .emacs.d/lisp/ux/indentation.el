;;; indentation.el --- Configure Emacs indentation behavior
;;; Commentary:
;;; Code:

(defun user--dtrt-indent-config ()
  "Initialize dtrt indent."
  (setq-default
   dtrt-indent-verbosity 0
   global-mode-string (delq 'dtrt-indent-mode-line-info global-mode-string)))


(defun user--indentation-config ()
  "Initialize Emacs indentation behavior."
  (use-package dtrt-indent
    :ensure t
    :config (user--dtrt-indent-config))
  (require-package '(:name smarttabs)))

(user--indentation-config)


(provide 'ux/indentation)
;;; indentation.el ends here
