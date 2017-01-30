;;; vala.el --- Vala mode support
;;; Commentary:
;;; Code:

(defun user--vala-mode-hook ()
  "Vala mode hook."
  ;; Bring in CEDET.
  (user--cedet-hook))


(defun user--vala-mode-config ()
  "Initialize Vala mode."
  (add-hook 'vala-mode-hook 'user--vala-mode-hook))

(with-executable 'valac
  (use-package vala-mode
    :ensure t
    :config (user--vala-mode-config)))


(provide 'modes/vala)
;;; vala.el ends here
