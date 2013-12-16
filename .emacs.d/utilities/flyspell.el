;;; flyspell.el --- spell checking on the fly
;;; Commentary:
;;; Code:

(defun user/flyspell-mode-hook ()
  "Hook for fly spell mode."
  ;;; (Bindings) ;;;
  (define-key user/code-map (kbd "s") 'ispell-word))


(defun user/flyspell-lazy-init ()
  "Initialize fly spell lazy."
  (flyspell-lazy-mode t))


(defun user/flyspell-init ()
  "Initialize fly spell."
  (when *has-aspell*
    (setq-default
     ispell-program-name "aspell"
     ispell-list-command "--list"
     ispell-extra-args '("--sug-mode=ultra")))

  (require-package '(:name flyspell-lazy
                           :type github
                           :pkgname "rolandwalker/flyspell-lazy"
                           :features (flyspell-lazy)
                           :after (user/flyspell-lazy-init)))
  (require-package '(:name auto-dictionary))

  (add-hook 'flyspell-mode-hook 'user/flyspell-mode-hook))

(user/flyspell-init)


(provide 'utilities/flyspell)
;;; flyspell.el ends here
