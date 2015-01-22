;;; scheme.el --- Set up Scheme mode
;;; Commentary:
;;; Code:

(defun user/scheme-mode-hook ()
  "Scheme mode hook."
  (user/cedet-hook)
  (semantic-default-scheme-setup)

  (user/gnu-global-enable)

  (enable-paredit-mode)

  (geiser-mode t))


(defun user/run-geiser ()
  "Run Geiser picking the current Scheme implementation."
  (interactive)
  (if (and (boundp 'geiser-scheme-implementation) geiser-scheme-implementation)
      (run-geiser geiser-scheme-implementation)
    (call-interactively 'run-geiser)))


(defun user/geiser-mode-hook ()
  "Geiser mode hook."
  (with-feature 'ac-geiser
    (ac-geiser-setup))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :run 'user/run-geiser)
  (user/bind-key-local :code :eval-buffer 'geiser-eval-buffer)
  (user/bind-key-local :code :eval-function 'geiser-eval-definition)
  (user/bind-key-local :code :eval-expression 'geiser-eval-last-sexp)
  (user/bind-key-local :code :eval-selection 'geiser-eval-region)
  (user/bind-key-local :code :macro-expand 'geiser-expand-definition))


(defun user/geiser-repl-mode-hook ()
  "Geiser REPL mode hook."
  (enable-paredit-mode)
  (rainbow-delimiters-mode t)

  (with-feature 'ac-geiser
    (ac-geiser-setup)))


(defun user/geiser-init ()
  "Initialize Geiser."
  (when (feature-p 'ac-geiser)
    (after-load 'geiser
      (add-ac-modes 'geiser-repl-mode)))

  ;;; (Hooks) ;;;
  (add-hook 'geiser-mode-hook 'user/geiser-mode-hook)
  (add-hook 'geiser-repl-mode-hook 'user/geiser-repl-mode-hook))


(defun user/quack-init ()
  "Initialize Quack."
  (setq-default
   ;; Use Emacs-style fontification.
   quack-fontify-style 'emacs))


(defun user/scheme-mode-init ()
  "Initialize Scheme mode."
  ;;; (Hooks) ;;;
  (add-hook 'scheme-mode-hook 'user/scheme-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name geiser :after (user/geiser-init)))
  (require-package '(:name quack :after (user/quack-init)))
  (require-package '(:name ac-geiser)))

(user/scheme-mode-init)


(provide 'modes/scheme)
;;; scheme.el ends here