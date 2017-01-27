;;; polymode.el --- Multiple major modes in a single buffer.
;;; Commentary:
;;; Code:

(defun user--polymode-config ()
  "Initialize Polymode.")

(req-package polymode
  :config (user--polymode-config))


(provide 'utilities/polymode)
;;; polymode.el ends here
