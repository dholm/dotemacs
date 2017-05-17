;;; vala.el --- Vala mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--vala-mode-hook ()
  "Vala mode hook."
  ;; Bring in CEDET.
  (user--cedet-hook))

(use-package vala-mode
  :if (executable-find "valac")
  :defer
  :init
  (add-hook 'vala-mode-hook 'user--vala-mode-hook))


(provide 'modes/vala)
;;; vala.el ends here
