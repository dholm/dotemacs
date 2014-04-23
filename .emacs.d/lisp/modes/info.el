;;; info.el --- Info mode.
;;; Commentary:
;;; Code:

(defun user/info-mode-hook ()
  "Info mode hook."
  (message "info mode")
  (when (feature-p 'info+)
    (require 'info+))

  (user/bind-key-local :nav :go-forward 'Info-history-forward)
  (user/bind-key-local :nav :go-back 'Info-history-back))


(defun user/info+-init ()
  "Initialize info+."
  (setq-default
   ;; Enable breadcrumbs in header line.
   Info-breadcrumbs-in-header-flag t
   Info-breadcrumbs-in-mode-line-mode nil))


(defun user/info-mode-init ()
  "Initialize info mode."
  (require-package '(:name info+ :type emacswiki :after (user/info+-init)))

  (add-hook 'Info-mode-hook 'user/info-mode-hook))

(user/info-mode-init)


(provide 'modes/info)
;;; info.el ends here
