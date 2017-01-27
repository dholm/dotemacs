;;; sx.el --- Set up Stack Exchange for Emacs
;;; Commentary:
;;; Code:

(defconst *user-sx-cache-directory*
 (path-join *user-cache-directory* "sx")
 "Path to user's Stack Exchange cache store.")

(defun user/sx-init ()
  "Initialize sx."
  (setq-default
   ;; Set up sx cache store.
   sx-cache-directory *user-sx-cache-directory*)

  (autoload 'sx-search "sx-load" nil t)

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :stack-exchange 'sx-search))

(req-package sx
  :config (user/sx-init))


(provide 'apps/sx)
;;; sx.el ends here
