;;; lisp.el --- Initializes Lisp modes
;;; Commentary:
;;; Code:

;;; (Mode Hooks) ;;;
(defun user/lisp-mode-common-hook ()
  "Lisp mode hook."
  (user/cedet-hook)
  (with-feature 'rainbow-delimiters
    (rainbow-delimiters-mode t))
  (with-feature 'paredit
    (enable-paredit-mode)
    (after-load 'diminish
      (diminish 'paredit-mode)))
  (with-feature 'redshank
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
  "Emacs Lisp mode hook."
  (user/lisp-mode-common-hook)

  (with-feature 'auto-compile
    (auto-compile-on-save-mode t)
    (auto-compile-on-load-mode t))
  (with-feature 'elisp-slime-nav
    (elisp-slime-nav-mode t)
    (after-load 'diminish
      (diminish 'elisp-slime-nav-mode)))
  (with-feature 'auto-complete-emacs-lisp
    (ac-emacs-lisp-mode-setup))
  (with-feature 'popwin
    (user/bind-key-local :util :popwin-messages 'popwin:messages))
  (with-feature 'macrostep
    (user/bind-key-local :util :macrostep-expand 'macrostep-expand))

  ;;; (Bindings) ;;;
  (user/bind-key-local :doc :reference 'elisp-index-search)
  (user/bind-key-local :doc :describe-function 'describe-function)
  (user/bind-key-local :doc :describe-variable 'describe-variable)

  (user/bind-key-local :debug :start 'debug)
  (user/bind-key-local :debug :break 'edebug-defun)
  (user/bind-key-local :debug :trace 'trace-function-background)
  (user/bind-key-local :debug :continue 'debugger-continue)
  (user/bind-key-local :debug :step 'debugger-step-through))


(defun user/ielm-mode-hook ()
  "Interactive Emacs Lisp mode hook."
  (user/emacs-lisp-mode-hook)
  (when (el-get-package-is-installed 'auto-complete)
    ;; Setup auto-complete.
    (ac-emacs-lisp-mode-setup)))


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

  (setq-default inferior-lisp-program
                (cond
                 ((executable-find "sbcl") "sbcl")
                 ((executable-find "lisp") "lisp")
                 ((executable-find "clisp") "clisp -K full")
                 (t inferior-lisp-program)))

  (add-ac-modes 'slime-repl-mode)

  (slime-setup '(slime-repl))

  (add-hook 'slime-mode-hook 'user/slime-mode-hook)
  (add-hook 'slime-repl-mode-hook 'user/slime-mode-hook))


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
  (require-package '(:name auto-compile))
  (require-package '(:name eldoc-eval :after (user/eldoc-eval-init)))

  (when (or (executable-find "sbcl")
            (executable-find "lisp")
            (executable-find "clisp"))
    (require-package '(:name slime :after (user/slime-init)))
    (require-package '(:name ac-slime)))

  ;;; (Hooks) ;;;
  (add-hook 'lisp-mode-hook 'user/lisp-mode-common-hook)
  (add-hook 'emacs-lisp-mode-hook 'user/emacs-lisp-mode-hook)
  (add-hook 'ielm-mode-hook 'user/ielm-mode-hook)
  (add-hook 'minibuffer-setup-hook 'user/minibuffer-setup-hook)

  (add-auto-mode 'emacs-lisp-mode "Carton$"))

(user/lisp-mode-init)


(provide 'modes/lisp)
;;; lisp.el ends here
