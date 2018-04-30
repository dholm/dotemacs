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


(use-package server
  ;; Emacs clients regularly causes Emacs to crash on Darwin.
  :if (not (eq system-type 'darwin))
  :ensure nil
  :hook (after-init-hook . user--server-after-init-hook)
  :config
  (use-package edit-server
    :if window-system))


(provide 'ux/server)
;;; server.el ends here
