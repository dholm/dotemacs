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
  (req-package dtrt-indent
    :config (user--dtrt-indent-config))
  (req-package smarttabs
    :loader :el-get))

(user--indentation-config)


(provide 'ux/indentation)
;;; indentation.el ends here
