;;; lisp.el --- Initializes Lisp modes
;;; Commentary:
;;; Code:

(defun user/lisp-mode-common-hook ()
  "Lisp mode hook."
  (setq-local
   ;; Indent using spaces.
   indent-tabs-mode nil)

  (user/cedet-hook)

  (user/gnu-global-enable)

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


(defun user/lisp-mode-hook ()
  "Lisp mode hook."
  (user/lisp-mode-common-hook))


(defun user/slime-mode-hook ()
  "SLIME mode hook."
  (when (el-get-package-is-installed 'ac-slime)
    (set-up-slime-ac)))


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


(defun user/lisp-mode-init ()
  "Initialize Lisp modes."
  ;;; (Hooks) ;;;
  (add-hook 'lisp-mode-hook 'user/lisp-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name rainbow-delimiters))
  (require-package '(:name paredit))
  (require-package '(:name redshank))

  (when (or (executable-find "sbcl")
            (executable-find "lisp")
            (executable-find "clisp"))
    (require-package '(:name slime :after (user/slime-init)))
    (require-package '(:name ac-slime))))

(user/lisp-mode-init)


(provide 'modes/lisp)
;;; lisp.el ends here
