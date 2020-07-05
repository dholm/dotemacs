;;; elisp.el --- Initializes Emacs Lisp modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--emacs-lisp-mode-hook ()
  "Emacs Lisp mode hook."
  (when (fboundp 'user--lisp-mode-common-hook)
    (user--lisp-mode-common-hook))

  (with-feature 'auto-compile
    (auto-compile-on-save-mode t)
    (auto-compile-on-load-mode t))

  (with-feature 'elisp-slime-nav
    (elisp-slime-nav-mode t))

  (with-feature 'eros
    (eros-mode t))

  (cond
   ((user/auto-complete-p)
    (ac-emacs-lisp-mode-setup))
   ((user/company-mode-p)
    (with-feature 'company-elisp
      (add-company-sources 'company-elisp))))

  ;;; (Bindings) ;;;
  (with-feature 'popwin
    (user/bind-key-local :util :popwin-messages 'popwin:messages))

  (with-feature 'macrostep
    (user/bind-key-local :code :macro-expand 'macrostep-expand))

  (user/bind-key-local :doc :reference 'elisp-index-search)
  (user/bind-key-local :doc :describe 'user/elisp-describe-thing-in-popup)
  (user/bind-key-local :doc :describe-function 'describe-function)
  (user/bind-key-local :doc :describe-variable 'describe-variable)

  (user/bind-key-local :debug :start 'debug)
  (user/bind-key-local :debug :break 'edebug-defun)
  (user/bind-key-local :debug :trace 'trace-function-background)
  (user/bind-key-local :debug :continue 'debugger-continue)
  (user/bind-key-local :debug :step 'debugger-step-through))


(defun user--ielm-mode-hook ()
  "Interactive Emacs Lisp mode hook."
  (user--emacs-lisp-mode-hook))


(defun user--minibuffer-setup-hook ()
  "Emacs minibuffer hook."
  (when (eq this-command 'eval-expression)
    (when (feature-p 'rainbow-delimiters)
      (rainbow-delimiters-mode))
    (when (feature-p 'paredit)
      (enable-paredit-mode))))


(defun user/elisp-describe-thing-in-popup ()
  "Describe elisp thing at point in a popup."
  (interactive)
  (with-feature 'popup
    (let* ((thing (symbol-at-point))
           (help-xref-following t)
           (description (with-temp-buffer
                          (help-mode)
                          (help-xref-interned thing)
                          (buffer-string))))
      (popup-tip description
                 :point (point)
                 :around t
                 :height 30
                 :scroll-bar t
                 :margin t))))

(use-package elisp-mode
  :ensure nil
  :mode ("Carton$" . emacs-lisp-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'user--emacs-lisp-mode-hook)
  (add-hook 'minibuffer-setup-hook 'user--minibuffer-setup-hook)
  :config
  (use-package ielm
    :init
    (add-hook 'ielm-mode-hook 'user--ielm-mode-hook)
    :config
    ;; Use auto-completion even in inferior elisp mode.
    (add-ac-modes 'inferior-emacs-lisp-mode))

  ;;; (Packages) ;;;
  (use-package macrostep)
  (use-package eros
    :config
    (when (eq default-terminal-coding-system 'utf-8)
      (validate-setq
       eros-eval-result-prefix "∴ ")))
  (use-package elisp-slime-nav
    :diminish elisp-slime-nav-mode)
  (use-package auto-compile)
  (use-package rainbow-delimiters)
  (use-package paredit
    :diminish paredit-mode)
  (use-package redshank
    :diminish redshank-mode)
  (use-package helm-elisp
    :ensure helm
    :bind (("C-c h a" . helm-apropos)
           ("C-c h l" . helm-locate-library))
    :config
    (validate-setq
     helm-apropos-fuzzy-match t
     helm-lisp-fuzzy-completion t))
  (use-package flycheck-package
    :config
    (flycheck-package-setup))
  (use-package flycheck-relint
    :config
    (flycheck-relint-setup))
  (use-package fontify-face
    :hook (emacs-lisp-mode-hook . fontify-face-mode))
  (use-package elpl
    :bind-wrap
    ((:key :code :repl) . elpl)))


(provide 'modes/elisp)
;;; elisp.el ends here
