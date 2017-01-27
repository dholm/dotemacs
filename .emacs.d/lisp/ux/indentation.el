;;; indentation.el --- Configure Emacs indentation behavior
;;; Commentary:
;;; Code:

(defun user/dtrt-indent-init ()
  "Initialize dtrt indent."
  (setq-default
   dtrt-indent-verbosity 0
   global-mode-string (delq 'dtrt-indent-mode-line-info global-mode-string)))


(defun user/indentation-init ()
  "Initialize Emacs indentation behavior."
  (req-package dtrt-indent
    :config (user/dtrt-indent-init))
  (req-package smarttabs
    :loader :el-get))

(user/indentation-init)


(provide 'ux/indentation)
;;; indentation.el ends here
