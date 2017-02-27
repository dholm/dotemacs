;;; tls.el --- Emacs TLS communication -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package tls
  :ensure nil
  :defer
  :config
  ;; Don't use validate-setq due to :inline not being supported.
  (setq
   ;; Default to OpenSSL, then fall back to GnuTLS.
   tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                 "gnutls-cli --priority secure256 --disable-extensions -p %p %h")))

(use-package starttls
  :ensure nil
  :defer
  :config
  (validate-setq
   ;; Default GnuTLS arguments.
   starttls-extra-arguments '("--priority" "secure256" "--disable-extensions")))


(provide 'utilities/tls)
;;; tls.el ends here
