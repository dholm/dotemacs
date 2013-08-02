;;; lisp --- initializes LISP modes
;;; Commentary:
;;; Code:

(require 'lib/utils)


(defun user/lisp-mode-hook ()
  "LISP mode hook."
  (try-eval (user/cedet-hook))
  (try-eval (rainbow-delimiters-mode))
  (try-eval (enable-paredit-mode))
  (try-eval (redshank-mode))
  (turn-on-eldoc-mode))

(defun user/emacs-lisp-mode-hook ()
  "Emacs LISP mode hook."
  (user/lisp-mode-hook)
  (try-eval (elisp-slime-nav-mode))
  (try-eval (ac-emacs-lisp-mode-setup))
  (local-set-key (kbd "C-c e") 'macrostep-expand))


;; Install mode hooks
(add-hook 'lisp-mode-hook 'user/lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'user/emacs-lisp-mode-hook)


;;; (Package Initialization) ;;;
(defun user/newlisp-mode-init ()
  "Initialize newlisp mode."
  (add-auto-mode 'newlisp-mode "\\.lsp$")
  (add-to-list 'interpreter-mode-alist '("newlisp" . newlisp-mode)))


(defun user/swank-newlisp-init ()
  "Initialize swank newlisp."
  (setq-default inferior-lisp-program "newlisp")
  (defun swank-newlisp-init (port-filename coding-system)
    (format "%S\n" `(swank:start-server ,port-filename)))
  (defvar swank-newlisp-filename "swank-newlisp.lsp")
  (defun slime-newlisp ()
    (interactive)
    (let ((slime-lisp-implementations
           `((newlisp ("newlisp" "-n" ,(locate-file swank-newlisp-filename load-path))
                      :init swank-newlisp-init
                      :coding-system utf-8-unix))))
      (slime 'newlisp))))


(defun user/auto-compile-init ()
  "Initialize auto-compile."
  (require 'auto-compile)
  (auto-compile-on-save-mode t)
  (auto-compile-on-load-mode t))


(require-package '(:name rainbow-delimiters))
(require-package '(:name paredit))
(require-package '(:name redshank))
(require-package '(:name macrostep
			 :type github
			 :pkgname "joddie/macrostep"))
(require-package '(:name auto-complete-emacs-lisp))
(require-package '(:name slime))
(require-package '(:name elisp-slime-nav))
(require-package '(:name newlisp-mode
			 :type github
			 :pkgname "kosh04/newlisp-mode"
			 :features newlisp-mode
			 :after (user/newlisp-mode-init)))
(require-package '(:name swank-newlisp
			 :type github
			 :pkgname "kosh04/swank-newlisp"
			 :depends (slime)
			 :after (user/swank-newlisp-init)))
(require-package '(:name auto-compile
			 :type github
			 :pkgname "tarsius/auto-compile"
			 :depends (packed)
			 :after (user/auto-compile-init)))


(provide 'modes/lisp)
;;; lisp.el ends here
