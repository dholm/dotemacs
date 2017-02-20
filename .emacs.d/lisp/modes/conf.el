;;; conf.el --- Initializes configuration mode
;;; Commentary:
;;; Code:

(defun user--conf-mode-hook ()
  "Configuration mode hook."
  (user--fundamental-mode-hook))

(use-package conf-mode
  :defer t
  :init
  (add-hook 'conf-mode-hook 'user--conf-mode-hook)
  :config
  ;;; (Packages) ;;;
  (with-executable 'nginx
    (use-package nginx-mode
      :defer t
      :init
      (add-auto-mode 'nginx-mode "etc/nginx/.*$"))))


(provide 'modes/conf)
;;; conf.el ends here
