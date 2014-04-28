;;; vc.el --- Emacs version control package support
;;; Commentary:
;;; Code:

(defun user/vc-find-file-hook ()
  "Find file hook for version controlled files."
  (when (vc-working-revision (buffer-file-name))
    ;; Automatically refresh version controlled files.
    (auto-revert-mode t)))


(defun user/vc-log-edit-hook ()
  "Version control log editing hook."
  ;; Enable org minor mode editing tools.
  (orgstruct-mode t)
  (orgtbl-mode t))


(defun user/vc-init ()
  "Initialize Emacs version control package."
  ;; Full frame annotations.
  (with-feature 'fullframe
    (fullframe vc-annotate vc-annotate-mode-quit-window nil))

  (add-hook 'find-file-hook 'user/vc-find-file-hook)

  ;;; (Packages) ;;;
  (require-package '(:name diff-hl)))

(user/vc-init)


(provide 'vcs/vc)
;;; vc.el ends here
