;;; org.el --- Org mode support
;;; Commentary:
;;; Code:

(defun user/org-mode-hook ()
  "Org mode hook.")


(defun user/org-mode-init ()
  "Initialize Lua mode."
  (require-package '(:name org-mode))

  (add-hook 'org-mode-hook 'user/org-mode-hook))

(user/org-mode-init)


(provide 'modes/org)
;;; org.el ends here
