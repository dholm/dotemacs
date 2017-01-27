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
  (req-package vala-mode
    :config (user/vala-mode-init)))


(provide 'modes/vala)
;;; vala.el ends here
