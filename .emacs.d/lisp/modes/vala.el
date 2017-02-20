;;; vala.el --- Vala mode support
;;; Commentary:
;;; Code:

(defun user--vala-mode-hook ()
  "Vala mode hook."
  ;; Bring in CEDET.
  (user--cedet-hook))

(with-executable 'valac
  (use-package vala-mode
    :defer t
    :init
    (add-hook 'vala-mode-hook 'user--vala-mode-hook)))


(provide 'modes/vala)
;;; vala.el ends here
