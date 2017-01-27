;;; elim.el --- instant messenger
;;; Commentary:
;;; Code:

(defun user--elim-config ()
  "Initialize elim."
  (setq-default
   lui-max-buffer-size 30000
   lui-flyspell-p t
   lui-flyspell-alist '(("." "american"))
   elim-directory (path-join *user-cache-directory* "elim"))

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :instant-messenger 'garak))

(when (and (pkg-config-has-p "libxml-2.0")
           (pkg-config-has-p "purple"))
  (req-package elim
    :loader :el-get
    :config (user--elim-config)))


(provide 'apps/elim)
;;; elim.el ends here
