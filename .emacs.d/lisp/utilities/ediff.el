;;; ediff.el --- Configuration for ediff
;;; Commentary:
;;; Code:

(defun user/ediff-mode-hook ()
  "Ediff mode hook."
  (setq
   ;; Don't wrap long lines.
   truncate-lines t))


(defun user/ediff-init ()
  "Initialize ediff."
  (setq-default
   ;; Ignore changes in whitespace.
   ediff-diff-options "-w"
   ediff-ignore-similar-regions t)

  ;; Go to first difference on start.
  (add-hook 'ediff-startup-hook 'ediff-next-difference))

(after-load 'ediff
  (user/ediff-init))


(provide 'utilities/ediff)
;;; ediff.el ends here
