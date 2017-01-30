;;; perl.el --- initializes Perl modes
;;; Commentary:
;;; Code:

(defun user--perl-mode-hook ()
  "Perl mode hook."
  (user/gnu-global-enable)

  ;; Enable YouCompleteMe.
  (user/ycmd-enable)

  (when (user/auto-complete-p)
    (with-feature 'perl-completion
      (add-ac-sources 'ac-source-perl-completion))))


(defun user--sepia-mode-hook ()
  "Sepia mode hook."
  (user--perl-mode-hook))


(defun user--perl-mode-config ()
  "Initialize Perl mode."
  (require-package '(:name cperl-mode))
  (require-package '(:name sepia))
  (use-package perl-completion
    :defer t)

  ;; Use Sepia as the default perl mode.
  (defalias 'perl-mode 'sepia-mode)

  (add-hook 'perl-mode-hook 'user--perl-mode-hook)
  (add-hook 'sepia-mode-hook 'user--sepia-mode-hook))

(with-executable 'perl
  (user--perl-mode-config))


(provide 'modes/perl)
;;; perl.el ends here
