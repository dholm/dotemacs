;;; perl.el --- initializes Perl modes
;;; Commentary:
;;; Code:

(defun user/perl-mode-hook ()
  "Perl mode hook."
  (auto-complete-mode t)
  (add-ac-sources 'ac-source-perl-completion)

  ;; Initialize PDE
  (user/pde-perl-mode-hook))


(defun user/pde-perl-mode-hook ()
  "PDE Perl mode hook."
  (with-feature 'pde-load
    (setq-default
     compilation-buffer-name-function 'pde-compilation-buffer-name)

    ;; chmod when saving
    (when (and buffer-file-name
               (not (string-match "\\.\\(pm\\|pod\\)$" (buffer-file-name))))
      (add-hook 'after-save-hook 'executable-chmod nil t))
    (set (make-local-variable 'compile-dwim-check-tools) nil)

    ;;; (Bindings) ;;;
    (user/bind-key-local :code :compile 'compile-dwim-compile)
    (user/bind-key-local :code :run 'compile-dwim-run)))


(defun user/perl-mode-init ()
  "Initialize Perl mode."
  (require-package '(:name perl-completion
                           :type emacswiki
                           :website "https://raw.github.com/emacsmirror/emacswiki.org/master/perl-completion.el"))
  (require-package '(:name pde))

  (add-hook 'perl-mode-hook 'user/perl-mode-hook))

(with-executable 'perl
  (user/perl-mode-init))


(provide 'modes/perl)
;;; perl.el ends here
