;;; (Initialization) ;;;
(require 'utilities/cedet)

(require-package (:name rainbow-delimiters))
(require-package (:name paredit))
(require-package (:name redshank))
(require-package (:name macrostep))
(require-package (:name auto-complete-emacs-lisp))
(require-package (:name slime))
(require-package (:name elisp-slime-nav))
(require-package (:name newlisp-mode))
(require-package (:name swank-newlisp))
(require-package (:name auto-compile :after (dholm/auto-compile-init)))


(setq inferior-lisp-program "newlisp")


(defun dholm/auto-compile-init ()
  (require 'auto-compile)
  (auto-compile-on-save-mode t)
  (auto-compile-on-load-mode t))


;;; (Code Conventions) ;;;
(defun dholm/lisp-mode-hook ()
  ;; Enable CEDET
  (dholm/cedet-hook)
  (when (featurep 'rainbow-delimiters)
    (rainbow-delimiters-mode))
  (when (featurep 'paredit)
    (enable-paredit-mode))
  (turn-on-eldoc-mode)
  (when (featurep 'redshank)
    (redshank-mode))
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode))

(defun dholm/emacs-lisp-mode-hook ()
  (dholm/lisp-mode-hook)
  (when (featurep 'elisp-slime-nav)
    (elisp-slime-nav-mode))
  (when (featurep 'auto-complete-emacs-lisp)
    (ac-emacs-lisp-mode-setup))
  (local-set-key (kbd "C-c e") 'macrostep-expand))

(add-hook 'lisp-mode-hook 'dholm/lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'dholm/emacs-lisp-mode-hook)


(provide 'modes/lisp)
