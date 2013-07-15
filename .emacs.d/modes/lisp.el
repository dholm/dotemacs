;;; (Initialization) ;;;
(require 'utilities/cedet)

(require-package (:name rainbow-delimiters))
(require-package (:name paredit))
(require-package (:name redshank))
(require-package (:name macrostep
                        :type github
                        :pkgname "joddie/macrostep"))
(require-package (:name auto-complete-emacs-lisp))
(require-package (:name slime))
(require-package (:name elisp-slime-nav))
(require-package (:name newlisp-mode
                        :type github
                        :pkgname "kosh04/newlisp-mode"
                        :features newlisp-mode
                        :post-init (progn
                                     (add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))
                                     (add-to-list 'interpreter-mode-alist '("newlisp" . newlisp-mode)))))
(require-package (:name swank-newlisp
                        :type github
                        :pkgname "kosh04/swank-newlisp"
                        :depends slime
                        :post-init (progn
                                     (defun swank-newlisp-init (port-filename coding-system)
                                       (format "%S\n" `(swank:start-server ,port-filename)))
                                     (defvar swank-newlisp-filename "swank-newlisp.lsp")
                                     (defun slime-newlisp ()
                                       (interactive)
                                       (let ((slime-lisp-implementations
                                              `((newlisp ("newlisp" "-n" ,(locate-file swank-newlisp-filename load-path))
                                                         :init swank-newlisp-init
                                                         :coding-system utf-8-unix))))
                                         (slime 'newlisp))))))
(require-package (:name auto-compile
                        :type github
                        :pkgname "tarsius/auto-compile"
                        :depends (packed)
                        :after (dholm/auto-compile-init)))


(setq inferior-lisp-program "newlisp")


(defun dholm/auto-compile-init ()
  (require 'auto-compile)
  (auto-compile-on-save-mode t)
  (auto-compile-on-load-mode t))


;;; (Code Conventions) ;;;
(defun dholm/lisp-mode-hook ()
  ;; Enable CEDET
  (dholm/cedet-hook)
  (unwind-protect
      (progn
        (rainbow-delimiters-mode)
        (enable-paredit-mode)
        (redshank-mode)))
  (turn-on-eldoc-mode)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode))

(defun dholm/emacs-lisp-mode-hook ()
  (dholm/lisp-mode-hook)
  (unwind-protect
      (progn
        (elisp-slime-nav-mode)
        (ac-emacs-lisp-mode-setup)))
  (local-set-key (kbd "C-c e") 'macrostep-expand))

(add-hook 'lisp-mode-hook 'dholm/lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'dholm/emacs-lisp-mode-hook)


(provide 'modes/lisp)
