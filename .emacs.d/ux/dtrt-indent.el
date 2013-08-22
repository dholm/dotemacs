
;;; dtrt-indent.el --- do the right thing indentation
;;; Commentary:
;;; Code:

(defun user/dtrt-indent-init ()
  "Initialize dtrt indent."
  (setq-default
   dtrt-indent-verbosity 0
   global-mode-string (delq 'dtrt-indent-mode-line-info global-mode-string)))

(require-package '(:name dtrt-indent :after (user/dtrt-indent-init)))


(provide 'ux/dtrt-indent)
;;; dtrt-indent.el ends here
