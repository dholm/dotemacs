;;; lisp.el --- initializes LISP modes
;;; Commentary:
;;; Code:

;;; (Mode Hooks) ;;;
(defun user/lisp-mode-common-hook ()
  "LISP mode hook."
  (user/cedet-hook)
  (when (require 'rainbow-delimiters nil :noerror)
    (rainbow-delimiters-mode t))
  (when (require 'paredit nil :noerror)
    (enable-paredit-mode)
    (after-load 'diminish
      (diminish 'paredit-mode)))
  (when (require 'redshank nil :noerror)
    (redshank-mode t)
    (after-load 'diminish
      (diminish 'redshank-mode)))
  (turn-on-eldoc-mode)
  (after-load 'diminish
    (diminish 'eldoc-mode))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :eval-buffer 'eval-buffer)
  (user/bind-key-local :code :eval-function 'eval-defun)
  (user/bind-key-local :code :eval-selection 'eval-region)
  (user/bind-key-local :code :eval-expression 'eval-last-sexp))


(defun user/emacs-lisp-mode-hook ()
  "Emacs LISP mode hook."
  (user/lisp-mode-common-hook)
  (when (require 'elisp-slime-nav nil :noerror)
    (elisp-slime-nav-mode t)
    (after-load 'diminish
      (diminish 'elisp-slime-nav-mode)))
  (when (require 'auto-complete-emacs-lisp nil :noerror)
    (ac-emacs-lisp-mode-setup))
  (when (require 'popwin nil :noerror)
    (user/bind-key-local :navigation :popwin-messages 'popwin:messages))
  (when (require 'macrostep nil :noerror)
    (user/bind-key-local :util :macrostep-expand 'macrostep-expand))

  ;;; (Bindings) ;;;
  (user/bind-key-local :docs :reference 'elisp-index-search)
  (user/bind-key-local :docs :describe-function 'describe-function)
  (user/bind-key-local :docs :describe-variable 'describe-variable))


(defun user/ielm-mode-hook ()
  "Interactive Emacs LISP mode hook."
  (user/emacs-lisp-mode-hook))


(defun user/minibuffer-setup-hook ()
  "Emacs minibuffer hook."
  (when (eq this-command 'eval-expression)
    (when (el-get-package-is-installed 'rainbow-delimiters)
      (rainbow-delimiters-mode))
    (when (el-get-package-is-installed 'paredit)
      (enable-paredit-mode))))


(defun user/slime-mode-hook ()
  "SLIME mode hook."
  (user/lisp-mode-common-hook)
  (when (el-get-package-is-installed 'ac-slime)
    (set-up-slime-ac)))


;;; (Initialization) ;;;
(defun user/slime-init ()
  "Initialize SLIME."
  (setq-default
   slime-protocol-version 'ignore
   slime-net-coding-system 'utf-8-unix
   slime-complete-symbol*-fancy t)

  (cond
   (*has-sbcl* (setq-default inferior-lisp-program "sbcl"))
   (*has-lisp* (setq-default inferior-lisp-program "lisp"))
   (*has-clisp* (setq-default inferior-lisp-program "clisp -K full")))

  (add-ac-modes 'slime-repl-mode)

  (slime-setup '(slime-repl))

  (add-hook 'slime-mode-hook 'user/slime-mode-hook)
  (add-hook 'slime-repl-mode-hook 'user/slime-mode-hook))


(defun user/auto-compile-init ()
  "Initialize auto-compile."
  (require 'auto-compile)
  (auto-compile-on-save-mode t)
  (auto-compile-on-load-mode t))


(defun user/eldoc-eval-init ()
  "Initialize eldoc eval."
  (require 'eldoc-eval)
  (eldoc-in-minibuffer-mode t))


(defun user/lisp-mode-init ()
  "Initialize Lisp modes."
  ;;; (Packages) ;;;
  (require-package '(:name rainbow-delimiters))
  (require-package '(:name paredit))
  (require-package '(:name redshank))
  (require-package '(:name macrostep))
  (require-package '(:name auto-complete-emacs-lisp))
  (require-package '(:name elisp-slime-nav))
  (require-package '(:name auto-compile :after (user/auto-compile-init)))
  (require-package '(:name eldoc-eval
                           :type github
                           :pkgname "thierryvolpiatto/eldoc-eval"
                           :after (user/eldoc-eval-init)))

  (when (or *has-sbcl* *has-lisp* *has-clisp*)
    (require-package '(:name slime :after (user/slime-init)))
    (require-package '(:name ac-slime)))

  ;;; (Bindings) ;;;
  (define-key user/code-map (kbd "e") 'eval-expression)

  ;;; (Hooks) ;;;
  (add-hook 'lisp-mode-hook 'user/lisp-mode-common-hook)
  (add-hook 'emacs-lisp-mode-hook 'user/emacs-lisp-mode-hook)
  (add-hook 'ielm-mode-hook 'user/ielm-mode-hook)
  (add-hook 'minibuffer-setup-hook 'user/minibuffer-setup-hook)

  (add-auto-mode 'emacs-lisp-mode "Carton$"))

(user/lisp-mode-init)


(provide 'modes/lisp)
;;; lisp.el ends here
