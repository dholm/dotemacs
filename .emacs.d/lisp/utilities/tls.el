;;; tls.el --- Emacs TLS communication
;;; Commentary:
;;; Code:

(defun user/tls-init ()
  "Initialize Emacs TLS communications."
  (setq-default
   ;; Default to OpenSSL, hten fall back to GnuTLS.
   tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                 "gnutls-cli --priority secure256 -p %p %h"
                 "gnutls-cli --priority secure256 -p %p %h")))

(user/tls-init)


(provide 'utilities/tls)
;;; tls.el ends here
