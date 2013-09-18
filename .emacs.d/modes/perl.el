;;; perl.el --- initializes Perl modes
;;; Commentary:
;;; Code:

(defconst *has-perl* (executable-find "perl"))


(defun user/perl-mode-hook ()
  "Perl mode hook."
  (auto-complete-mode t)
  (add-ac-sources 'ac-source-perl-completion)

  ;; Initialize PDE
  (user/pde-perl-mode-hook)

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.pl")))


(defun user/pde-perl-mode-hook ()
  "PDE Perl mode hook."
  (require 'pde-load)

  ;; chmod when saving
  (when (and buffer-file-name
             (not (string-match "\\.\\(pm\\|pod\\)$" (buffer-file-name))))
    (add-hook 'after-save-hook 'executable-chmod nil t))
  (set (make-local-variable 'compile-dwim-check-tools) nil)

  ;;; (Bindings) ;;;
  (define-key user/code-map (kbd "c") 'compile-dwim-compile)
  (define-key user/code-map (kbd "r") 'compile-dwim-run)
  (setq compilation-buffer-name-function 'pde-compilation-buffer-name))


(defun user/perl-mode-init ()
  "Initialize Perl mode."
  (require-package '(:name perl-completion
                           :type emacswiki
                           :website "https://raw.github.com/emacsmirror/emacswiki.org/master/perl-completion.el"))
  (require-package '(:name pde))

  (add-hook 'perl-mode-hook 'user/perl-mode-hook))

(when *has-perl*
  (user/perl-mode-init))


(provide 'modes/perl)
;;; perl.el ends here
