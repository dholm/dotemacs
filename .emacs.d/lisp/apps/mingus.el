;;; mingus.el --- MPD frontend for Emacs
;;; Commentary:
;;; Code:

(defun user--mingus-config ()
  "Initialize Mingus."
  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :music 'mingus))

(when (fboundp 'define-fringe-bitmap)
  (req-package mingus
    :config (user--mingus-config)))


(provide 'apps/mingus)
;;; mingus.el ends here
