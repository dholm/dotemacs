;;; pkg-config.el --- pkg-config support
;;; Commentary:
;;; Code:

(defun pkg-config-has-p (package)
  "Check if PACKAGE is available."
  (eq (call-process-shell-command "pkg-config" nil nil nil "--exists" package) 0))


(provide 'lib/pkg-config)
;;; pkg-config.el ends here
