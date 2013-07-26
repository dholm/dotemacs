;;; lisp --- initializes LISP modes
;;; Commentary:
;;; Code:

(require 'lib/utils)


(defun dholm/lisp-mode-hook ()
  "LISP mode hook."
  (setq-default
   ;; Indent using spaces
   indent-tabs-mode nil)
  (try-eval (dholm/cedet-hook))
  (try-eval (rainbow-delimiters-mode))
  (try-eval (enable-paredit-mode))
  (try-eval (redshank-mode))
  (turn-on-eldoc-mode)
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode))


(defun dholm/emacs-lisp-mode-hook ()
  "Emacs LISP mode hook."
  (dholm/lisp-mode-hook)
  (try-eval (elisp-slime-nav-mode))
  (try-eval (ac-emacs-lisp-mode-setup))
  (local-set-key (kbd "C-c e") 'macrostep-expand))


;; Install mode hooks
(add-hook 'lisp-mode-hook 'dholm/lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'dholm/emacs-lisp-mode-hook)


;;; (Package Initialization) ;;;
(defun dholm/newlisp-mode-init ()
  "Initialize newlisp mode."
  (add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))
  (add-to-list 'interpreter-mode-alist '("newlisp" . newlisp-mode)))


(defun dholm/swank-newlisp-init ()
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


(defun dholm/auto-compile-init ()
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
			 :after (dholm/newlisp-mode-init)))
(require-package '(:name swank-newlisp
			 :type github
			 :pkgname "kosh04/swank-newlisp"
			 :depends (slime)
			 :after (dholm/swank-newlisp-init)))
(require-package '(:name auto-compile
			 :type github
			 :pkgname "tarsius/auto-compile"
			 :depends (packed)
			 :after (dholm/auto-compile-init)))


(provide 'modes/lisp)
;;; lisp.el ends here
