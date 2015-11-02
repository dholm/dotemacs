;;; perl.el --- initializes Perl modes
;;; Commentary:
;;; Code:

(defun user/perl-mode-hook ()
  "Perl mode hook."
  (user/gnu-global-enable)

  ;; Enable YouCompleteMe.
  (user/ycmd-enable)

  (when (user/auto-complete-p)
    (with-feature 'perl-completion
      (add-ac-sources 'ac-source-perl-completion))))


(defun user/sepia-mode-hook ()
  "Sepia mode hook."
  (user/perl-mode-hook))


(defun user/perl-mode-init ()
  "Initialize Perl mode."
  (require-package '(:name cperl-mode))
  (require-package '(:name sepia))
  (require-package '(:name perl-completion
                           :type emacswiki
                           :website "https://raw.github.com/emacsmirror/emacswiki.org/master/perl-completion.el"))

  ;; Use Sepia as the default perl mode.
  (defalias 'perl-mode 'sepia-mode)

  (add-hook 'perl-mode-hook 'user/perl-mode-hook)
  (add-hook 'sepia-mode-hook 'user/sepia-mode-hook))

(with-executable 'perl
  (user/perl-mode-init))


(provide 'modes/perl)
;;; perl.el ends here
