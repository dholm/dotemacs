;;; indentation.el --- Configure Emacs indentation behavior
;;; Commentary:
;;; Code:

(defun user/indentation-init ()
  "Initialize Emacs indentation behavior."
  (require-package '(:name dtrt-indent :after (user/dtrt-indent-init)))
  (require-package '(:name smarttabs)))


(defun user/dtrt-indent-init ()
  "Initialize dtrt indent."
  (setq-default
   dtrt-indent-verbosity 0
   global-mode-string (delq 'dtrt-indent-mode-line-info global-mode-string)))


(user/indentation-init)


(provide 'ux/indentation)
;;; indentation.el ends here
