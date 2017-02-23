;;; sx.el --- Set up Stack Exchange for Emacs
;;; Commentary:
;;; Code:

(defconst *user-sx-cache-directory*
 (path-join *user-cache-directory* "sx")
 "Path to user's Stack Exchange cache store.")

(use-package sx
  :commands sx-search
  :init
  (autoload 'sx-search "sx-load" nil t)
  (user/bind-key-global :apps :stack-exchange 'sx-search)
  :config
  (validate-setq
   ;; Set up sx cache store.
   sx-cache-directory *user-sx-cache-directory*))


(provide 'apps/sx)
;;; sx.el ends here
