;;; flyspell.el --- spell checking on the fly
;;; Commentary:
;;; Code:

(defun user/flyspell-mode-hook ()
  "Hook for fly spell mode.")

(defun user/flyspell-init ()
  "Initialize fly spell."
  (when *has-aspell*
    (setq-default
     ispell-program-name "aspell"
     ispell-list-command "--list"
     ispell-extra-args '("--sug-mode=ultra")))

  (add-hook 'flyspell-mode-hook 'user/flyspell-mode-hook))

(after-load 'flyspell
  (user/flyspell-init))

(defun user/flyspell-lazy-init ()
  "Initialize fly spell lazy."
  (flyspell-lazy-mode t))

(require-package '(:name flyspell-lazy
                         :type github
                         :pkgname "rolandwalker/flyspell-lazy"
                         :features (flyspell-lazy)
                         :after (user/flyspell-lazy-init)))


(provide 'utilities/flyspell)
;;; flyspell.el ends here
