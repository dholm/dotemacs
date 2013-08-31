;;; lisp.el --- initializes LISP modes
;;; Commentary:
;;; Code:

;;; (Mode Hooks) ;;;
(defun user/lisp-mode-hook ()
  "LISP mode hook."
  (when (el-get-package-is-installed 'cedet)
    (user/cedet-hook))
  (when (el-get-package-is-installed 'rainbow-delimiters)
    (rainbow-delimiters-mode t))
  (when (el-get-package-is-installed 'paredit)
    (enable-paredit-mode)
    (after-load 'diminish
      (diminish 'paredit-mode)))
  (when (el-get-package-is-installed 'redshank)
    (redshank-mode t))
  (turn-on-eldoc-mode))

(defun user/emacs-lisp-mode-hook ()
  "Emacs LISP mode hook."
  (user/lisp-mode-hook)
  (when (el-get-package-is-installed 'elisp-slime-nav)
    (elisp-slime-nav-mode t)
    (after-load 'diminish
      (diminish 'elisp-slime-nav-mode)))
  (when (el-get-package-is-installed 'auto-complete-emacs-lisp)
    (ac-emacs-lisp-mode-setup))
  (when (el-get-package-is-installed 'popwin)
    (define-key user/navigation-map (kbd "m") 'popwin:messages))
  (when (el-get-package-is-installed 'macrostep)
    (define-key user/navigation-map (kbd "e") 'macrostep-expand)))

(defun user/minibuffer-setup-hook ()
  "Emacs minibuffer hook."
  (when (eq this-command 'eval-expression)
    (when (el-get-package-is-installed 'rainbow-delimiters)
      (rainbow-delimiters-mode))
    (when (el-get-package-is-installed 'paredit)
      (enable-paredit-mode))))


;;; (Initialization) ;;;
(defun user/newlisp-mode-init ()
  "Initialize newlisp mode."
  (add-auto-mode 'newlisp-mode "\\.lsp$")
  (add-interpreter-mode 'newlisp-mode "newlisp"))


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


(defun user/lisp-mode-init ()
  "Initialize Lisp modes."
  ;;; (Packages) ;;;
  (require-package '(:name rainbow-delimiters))
  (require-package '(:name paredit))
  (require-package '(:name redshank))
  (require-package '(:name macrostep))
  (require-package '(:name auto-complete-emacs-lisp))
  (require-package '(:name slime))
  (require-package '(:name elisp-slime-nav))
  (require-package '(:name newlisp-mode :after (user/newlisp-mode-init)))
  (require-package '(:name swank-newlisp :after (user/swank-newlisp-init)))
  (require-package '(:name auto-compile :after (user/auto-compile-init)))

  ;;; (Bindings) ;;;
  (define-key user/code-map (kbd "e") 'eval-expression)

  ;;; (Hooks) ;;;
  (add-hook 'lisp-mode-hook 'user/lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'user/emacs-lisp-mode-hook)
  (add-hook 'minibuffer-setup-hook 'user/minibuffer-setup-hook)

  (add-auto-mode 'emacs-lisp-mode "Carton$"))

(user/lisp-mode-init)


(provide 'modes/lisp)
;;; lisp.el ends here
