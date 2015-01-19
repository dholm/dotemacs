;;; tls.el --- Emacs TLS communication
;;; Commentary:
;;; Code:

(defun user/tls-init ()
  "Initialize Emacs TLS communications."
  (setq-default
   ;; Default GnuTLS arguments.
   starttls-extra-arguments '("--priority" "secure256" "--disable-extensions")
   ;; Default to OpenSSL, then fall back to GnuTLS.
   tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                 "gnutls-cli --priority secure256 --disable-extensions -p %p %h")))

(user/tls-init)


(provide 'utilities/tls)
;;; tls.el ends here
