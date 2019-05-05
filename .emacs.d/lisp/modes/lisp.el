;;; lisp.el --- Initializes Lisp modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--lisp-mode-common-hook ()
  "Lisp mode hook."
  (setq-local
   ;; Indent using spaces.
   indent-tabs-mode nil)

  (user--cedet-hook)

  (user/gnu-global-enable)

  (with-feature 'rainbow-delimiters
    (rainbow-delimiters-mode t))

  (with-feature 'paredit
    (enable-paredit-mode))

  (with-feature 'redshank
    (redshank-mode t))

  (with-feature 'easy-escape
    (easy-escape-minor-mode t))

  (turn-on-eldoc-mode)

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :eval-buffer 'eval-buffer)
  (user/bind-key-local :code :eval-function 'eval-defun)
  (user/bind-key-local :code :eval-selection 'eval-region)
  (user/bind-key-local :code :eval-expression 'eval-last-sexp))


(defun user--lisp-mode-hook ()
  "Lisp mode hook."
  (user--lisp-mode-common-hook))


(defun user--slime-mode-hook ()
  "SLIME mode hook."
  (when (feature-p 'ac-slime)
    (set-up-slime-ac)))

(use-package lisp-mode
  :ensure nil
  :defer
  :hook (lisp-mode-hook . user--lisp-mode-hook)
  :config
  :config
  (use-package slime
    :hook
    ((slime-mode-hook . user--slime-mode-hook)
     (slime-repl-mode-hook . user--slime-mode-hook))
    :config
    (validate-setq
     slime-protocol-version 'ignore
     slime-net-coding-system 'utf-8-unix
     ;; Set the preferred available inferior lisp program.
     inferior-lisp-program
     (cond
      ((executable-find "sbcl") "sbcl")
      ((executable-find "lisp") "lisp")
      ((executable-find "clisp") "clisp -K full")
      (t inferior-lisp-program)))

    (use-package slime-c-p-c
      :ensure slime
      :defer
      :config
      (validate-setq
       slime-complete-symbol*-fancy t))

    (slime-setup '(slime-repl))

    (use-package ac-slime
      :after (auto-complete)
      :config
      (add-ac-modes 'slime-repl-mode))

    (use-package slime-repl-ansi-color
      :hook (slime-mode-hook . slime-repl-ansi-color-mode)))

  (use-package easy-escape
    :diminish easy-escape-minor-mode))


(provide 'modes/lisp)
;;; lisp.el ends here
