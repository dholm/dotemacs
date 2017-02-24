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


(use-package sepia
  :quelpa (sepia
           :fetcher git
           :url "http://repo.or.cz/sepia.git")
  :defer t
  :init
  ;; Use Sepia as the default perl mode.
  (defalias 'perl-mode 'sepia-mode)
  :config
  ;;; (Packages) ;;;
  (use-package perl-completion
    :requires anything
    :defer t)

  ;;; (Hooks) ;;;
  (add-hook 'sepia-mode-hook 'user--sepia-mode-hook))

(use-package perl-mode
  :defer t
  :init
  (add-hook 'perl-mode-hook 'user--perl-mode-hook))


(provide 'modes/perl)
;;; perl.el ends here
