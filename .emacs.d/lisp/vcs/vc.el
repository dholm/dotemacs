;;; vc.el --- Emacs version control package support -*- lexical-binding: t; -*-
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
  (orgtbl-mode t))

(use-package vc-annotate
  :ensure nil
  :defer
  :config
  (with-feature 'fullframe
    ;; Full frame annotations.
    (fullframe vc-annotate vc-annotate-mode-quit-window nil)))

(use-package autorevert
  :ensure nil
  :defer
  :diminish auto-revert-mode
  :config
  (with-eval-after-load 'tramp
    (validate-setq auto-revert-remote-files t)))

(add-hook 'find-file-hook 'user--vc-find-file-hook)


(provide 'vcs/vc)
;;; vc.el ends here
