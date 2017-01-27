;;; vc.el --- Emacs version control package support
;;; Commentary:
;;; Code:

(defun user--vc-find-file-hook ()
  "Find file hook for version controlled files."
  (with-feature 'vc
    (when (vc-working-revision (buffer-file-name))
      ;; Automatically refresh version controlled files.
      (auto-revert-mode t))))


(defun user--vc-log-edit-hook ()
  "Version control log editing hook."
  ;; Limited whitespace style.
  (setq-local whitespace-style '(face lines-tail))

  ;; Set maximum line length.
  (setq fill-column 72)
  (setq-local whitespace-line-column 72)

  ;; Enable org minor mode editing tools.
  (orgstruct-mode t)
  (orgtbl-mode t)

  ;;; (Bindings) ;;;
  ;; Ensure C-x # is used to save and close message.
  (local-set-key (kbd "C-x #") 'user/server-save))


(defun user--vc-config ()
  "Initialize Emacs version control package."
  (after-load 'vc-annotate
    (with-feature 'fullframe
      ;; Full frame annotations.
      (fullframe vc-annotate vc-annotate-mode-quit-window nil)))

  (after-load 'autorevert
    (after-load 'tramp
      (setq-default auto-revert-remote-files t))
    (after-load 'diminish
      (diminish 'auto-revert-mode)))

  (add-hook 'find-file-hook 'user--vc-find-file-hook))

(user--vc-config)


(provide 'vcs/vc)
;;; vc.el ends here
