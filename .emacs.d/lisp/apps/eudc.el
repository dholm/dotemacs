;;; eudc.el --- Configure the Emacs Unified Directory Client
;;; Commentary:
;;; Code:

(defun user/eudc-init ()
  "Initialize EUDC."
  (setq-default
   ;; Only return default attributes for current server.
   eudc-default-return-attributes nil
   ;; Do not ignore incomplete results.
   eudc-strict-return-matches nil
   ;; Server hotlist store.
   eudc-options-file (path-join *user-cache-directory* "eudc-options")
   ;; Default `ldapsearch' parameters.
   ldap-ldapsearch-args '("-tt" "-LLL" "-x"))

  (after-load 'ldap
    (eudc-protocol-set
     'eudc-inline-expansion-format
     '("%s <%s>" displayName email)
     'ldap)
    (eudc-protocol-set
     'eudc-inline-query-format
     '((cn) (mail) (cn cn) (cn cn cn) (sn) (uid) (givenName) (givenName name) (name))
     'ldap))

  (after-load 'bbdb
    (eudc-protocol-set
     'eudc-inline-expansion-format
     '("%s %s <%s>" firstname lastname net)
     'bbdb)
    (eudc-protocol-set
     'eudc-inline-query-format
     '((name) (firstname) (lastname) (firstname lastname))
     'bbdb)

    ;; Default to BBDB.
    (eudc-set-server "localhost" 'bbdb t)
    (add-to-list 'eudc-server-hotlist '("localhost" . bbdb))))

(after-load 'eudc
  (user/eudc-init))


(provide 'apps/eudc)
;;; eudc.el ends here
