;;; conf.el --- Initializes configuration mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--conf-mode-hook ()
  "Configuration mode hook."
  (user--fundamental-mode-hook))

(use-package conf-mode
  :defer
  :init
  (add-hook 'conf-mode-hook 'user--conf-mode-hook)
  :config
  ;;; (Packages) ;;;
  (use-package nginx-mode
    :if (executable-find "nginx")
    :defer
    :init
    (add-auto-mode 'nginx-mode "etc/nginx/.*$")))


(provide 'modes/conf)
;;; conf.el ends here
