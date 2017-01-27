;;; prodigy.el --- Emacs service manager.
;;; Commentary:
;;; Code:

(defun user/prodigy-init ()
  "Initialize prodigy."
  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :services 'prodigy))

(use-package prodigy
  :defer t
  :config (user/prodigy-init))


(provide 'apps/prodigy)
;;; prodigy.el ends here
