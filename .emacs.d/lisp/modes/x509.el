;;; x509 --- Initializes x509 mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package x509-mode
  :mode "\\.\\(der\\|crt\\|pem\\)$"
  :magic "-----BEGIN CERTIFICATE-----")


(provide 'modes/x509)
;;; x509.el ends here
