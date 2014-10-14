;;; vala.el --- Vala mode support
;;; Commentary:
;;; Code:

(defun user/vala-mode-hook ()
  "Vala mode hook."
  ;; Bring in CEDET.
  (user/cedet-hook))


(defun user/vala-mode-init ()
  "Initialize Vala mode."
  (add-hook 'vala-mode-hook 'user/vala-mode-hook))

(with-executable 'valac
  (require-package '(:name vala-mode :after (user/vala-mode-init))))


(provide 'modes/vala)
;;; vala.el ends here
