;;; flyspell.el --- spell checking on the fly
;;; Commentary:
;;; Code:

(defconst *has-aspell* (executable-find "aspell"))

(defun user/flyspell-mode-hook ()
  "Hook for fly spell mode."
  (deferred-flyspell:install-hooks))

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

(require-package '(:name deferred-flyspell))


(provide 'utilities/flyspell)
;;; flyspell.el ends here
