;;; ttcn.el --- initializes TTCN modes
;;; Commentary:
;;; Code:

(defun user--ttcn3-mode-hook ()
  "TTCN mode hook."
  ;; Separate camel-case into separate words
  (subword-mode t))


(defun user--ttcn-mode-config ()
  "Initialize TTCN mode."
  (add-hook 'ttcn3-mode-hook 'user--ttcn3-mode-hook))


(req-package ttcn-mode
  :loader :el-get
  :config (user--ttcn-mode-config))


(provide 'modes/ttcn)
;;; ttcn.el ends here
