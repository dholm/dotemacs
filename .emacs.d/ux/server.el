;;; server.el --- Emacs server setup
;;; Commentary:
;;; Code:

(require 'server)

(defun user/server-init ()
  "Initialize Emacs server."
  (unless (server-running-p)
    (server-start)))

(add-hook 'after-init-hook 'user/server-init)


(provide 'ux/server)
;;; server.el ends here
