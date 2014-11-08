;;; elisp.el --- Initializes Emacs Lisp modes
;;; Commentary:
;;; Code:

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

  ;;; (Bindings) ;;;
  (with-feature 'popwin
    (user/bind-key-local :util :popwin-messages 'popwin:messages))

  (with-feature 'macrostep
    (user/bind-key-local :code :macro-expand 'macrostep-expand))

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
  (user/emacs-lisp-mode-hook))


(defun user/minibuffer-setup-hook ()
  "Emacs minibuffer hook."
  (when (eq this-command 'eval-expression)
    (when (el-get-package-is-installed 'rainbow-delimiters)
      (rainbow-delimiters-mode))
    (when (el-get-package-is-installed 'paredit)
      (enable-paredit-mode))))


(defun user/eldoc-eval-init ()
  "Initialize eldoc eval."
  (eldoc-in-minibuffer-mode t))


(defun user/ielm-init ()
  "Initialize interactive elisp mode."
  ;; Use auto-completion even in inferior elisp mode.
  (add-ac-modes 'inferior-emacs-lisp-mode)

  (add-hook 'ielm-mode-hook 'user/ielm-mode-hook))


(defun user/elisp-mode-init ()
  "Initialize Emacs Lisp modes."
  (after-load 'ielm
    (user/ielm-init))

  ;;; (Hooks) ;;;
  (add-hook 'emacs-lisp-mode-hook 'user/emacs-lisp-mode-hook)
  (add-hook 'minibuffer-setup-hook 'user/minibuffer-setup-hook)

  (add-auto-mode 'emacs-lisp-mode "Carton$")

  ;;; (Packages) ;;;
  (require-package '(:name macrostep))
  (require-package '(:name auto-complete-emacs-lisp))
  (require-package '(:name elisp-slime-nav))
  (require-package '(:name auto-compile))
  (require-package '(:name eldoc-eval :after (user/eldoc-eval-init))))

(after-load 'modes/lisp
  (user/elisp-mode-init))


(provide 'modes/elisp)
;;; elisp.el ends here
