;;; flyspell.el --- spell checking on the fly
;;; Commentary:
;;; Code:

(defconst *has-aspell* (executable-find "aspell"))

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

(defun user/deferred-flyspell-init ()
  "Initialize deferred flyspell."
  (add-hook 'flyspell-mode-hook 'deferred-flyspell:install-hooks))

(after-load 'flyspell
  (user/flyspell-init))

(require-package '(:name deferred-flyspell :after (user/deferred-flyspell-init)))


(provide 'utilities/flyspell)
;;; flyspell.el ends here
