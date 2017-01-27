;;; prodigy.el --- Emacs service manager.
;;; Commentary:
;;; Code:

(defun user--prodigy-config ()
  "Initialize prodigy."
  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :services 'prodigy))

(req-package prodigy
  :config (user--prodigy-config))


(provide 'apps/prodigy)
;;; prodigy.el ends here
