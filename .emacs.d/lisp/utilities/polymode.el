;;; polymode.el --- Multiple major modes in a single buffer.
;;; Commentary:
;;; Code:

(defun user/polymode-init ()
  "Initialize Polymode.")

(require-package '(:name polymode :after (user/polymode-init)))


(provide 'utilities/polymode)
;;; polymode.el ends here
