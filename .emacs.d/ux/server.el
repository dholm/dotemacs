;;; server.el --- Emacs server setup
;;; Commentary:
;;; Code:

(require 'server)

(defun user/server-init ()
  "Initialize Emacs server."
  (setq-default
   ;; Use TCP instead of local sockets.
   server-use-tcp t
   ;; Put TCP authentication files in cache directory.
   server-auth-dir (path-join *user-cache-directory* "server"))

  (unless (server-running-p)
    (server-start)))

(add-hook 'after-init-hook 'user/server-init)


(provide 'ux/server)
;;; server.el ends here
