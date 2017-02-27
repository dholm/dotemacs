;;; server.el --- Emacs server setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/server-save ()
  "Save and quickly exit from server edit mode."
  (interactive)
  (save-buffer)
  (server-edit))


(defun user--server-after-init-hook ()
  "Initialize Emacs server after init has completed."
  (with-feature 'server
    (unless (server-running-p)
      (server-start)

      (when (and (display-graphic-p)
                 (feature-p 'edit-server))
        (edit-server-start)))))


(defun user--server-config ()
  "Initialize Emacs server functions."
  (add-hook 'user--after-init-hook 'user--server-after-init-hook)

  (use-package edit-server
    :if window-system))

(unless (eq system-type 'darwin)
  ;; Emacs clients regularly causes Emacs to crash on Darwin.
  (user--server-config))


(provide 'ux/server)
;;; server.el ends here
