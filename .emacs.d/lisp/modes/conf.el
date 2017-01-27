;;; conf.el --- Initializes configuration mode
;;; Commentary:
;;; Code:

(defun user--conf-mode-hook ()
  "Configuration mode hook."
  (user--fundamental-mode-hook))


(defun user--nginx-mode-config ()
  "Initialize nginx mode."
  (add-auto-mode 'nginx-mode "etc/nginx/.*$"))


(defun user--conf-mode-config ()
  "Initialize assembly mode."
  (add-hook 'conf-mode-hook 'user--conf-mode-hook)

  ;;; (Packages) ;;;
  (with-executable 'nginx
    (req-package nginx-mode
      :config (user--nginx-mode-config))))

(user--conf-mode-config)


(provide 'modes/conf)
;;; conf.el ends here
