;;; perl.el --- initializes Perl modes
;;; Commentary:
;;; Code:

(defconst *has-perl* (executable-find "perl"))


(defun user/perl-mode-hook ()
  "Perl mode hook."
  (auto-complete-mode t)
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-perl-completion)))
  ;; Initialize PDE
  (user/pde-perl-mode-hook)

  ;; Register file types with find-file-in-project
  (when (el-get-package-is-installed 'find-file-in-project)
    (user/ffip-local-patterns "*.pl")))


(defun user/pde-perl-mode-hook ()
  "PDE Perl mode hook."
  (require 'pde-load)

  (local-set-key (kbd "C-c s") 'compile-dwim-compile)
  (local-set-key (kbd "C-c r") 'compile-dwim-run)
  (setq compilation-buffer-name-function 'pde-compilation-buffer-name)

  ;; chmod when saving
  (when (and buffer-file-name
             (not (string-match "\\.\\(pm\\|pod\\)$" (buffer-file-name))))
    (add-hook 'after-save-hook 'executable-chmod nil t))
  (set (make-local-variable 'compile-dwim-check-tools) nil))


(defun user/perl-mode-init ()
  "Initialize Perl mode."
  (require-package '(:name perl-completion
                           :type emacswiki
                           :website "https://raw.github.com/emacsmirror/emacswiki.org/master/perl-completion.el"))
  (require-package '(:name pde
                           :type github
                           :pkgname "wenbinye/emacs-pde"
                           :load-path ("lisp")))

  (add-hook 'perl-mode-hook 'user/perl-mode-hook))

(when *has-perl*
  (user/perl-mode-init))


(provide 'modes/perl)
;;; perl.el ends here
