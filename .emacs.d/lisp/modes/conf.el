;;; conf.el --- Initializes configuration mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--conf-mode-hook ()
  "Configuration mode hook."
  (user--fundamental-mode-hook))

(use-package conf-mode
  :defer
  :hook (conf-mode-hook . user--conf-mode-hook)
  :config
  (use-package nginx-mode
    :if (executable-find "nginx")
    :defer
    :mode "etc/nginx/.*$"
    :config
    (use-package company-nginx
      :hook (nginx-mode-hook . company-nginx-keywords))))


(provide 'modes/conf)
;;; conf.el ends here
