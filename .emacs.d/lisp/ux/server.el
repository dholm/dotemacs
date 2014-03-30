;;; server.el --- Emacs server setup
;;; Commentary:
;;; Code:

(defun user/server-after-init-hook ()
  "Initialize Emacs server after init has completed."
  (with-feature 'server
    (unless (server-running-p)
      (server-start)

      (when (and (display-graphic-p)
                 (el-get-package-is-installed 'edit-server))
        (edit-server-start)))))


(defun user/server-init ()
  "Initialize Emacs server functions."
  (add-hook 'after-init-hook 'user/server-after-init-hook)

  (when (display-graphic-p)
    (require-package '(:name edit-server))))

(user/server-init)


(provide 'ux/server)
;;; server.el ends here
