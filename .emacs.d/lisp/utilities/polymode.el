;;; polymode.el --- Multiple major modes in a single buffer.
;;; Commentary:
;;; Code:

(defun user/polymode-init ()
  "Initialize Polymode.")

(use-package polymode
  :ensure t
  :config (user/polymode-init))


(provide 'utilities/polymode)
;;; polymode.el ends here
