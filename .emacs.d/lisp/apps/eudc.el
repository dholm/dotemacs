;;; eudc.el --- Configure the Emacs Unified Directory Client
;;; Commentary:
;;; Code:

(defun user/eudc-expand-inline ()
  "Expand entry at point using EUDC."
  (interactive)
  (cond
   ((eq eudc-protocol 'ldap)
    (progn
      (unless (condition-case nil
                  (eudc-expand-inline)
                (error nil)))))
   (t (eudc-expand-inline)))
  ;; Remove metadata from email after expansion.
  (when (re-search-backward " {.*}" nil t)
    (replace-match "")))


(use-package eudc
  :config
  (validate-setq
   ;; Only return default attributes for current server.
   eudc-default-return-attributes nil
   ;; Do not ignore incomplete results.
   eudc-strict-return-matches nil
   ;; Server hotlist store.
   eudc-options-file (path-join *user-cache-directory* "eudc-options"))

  (after-load 'ldap
    (validate-setq
     ;; Default `ldapsearch' parameters.
     ldap-ldapsearch-args '("-tt" "-LLL" "-x"))

    ;; Treat the displayName attribute as a string.
    (add-to-list 'ldap-attribute-syntaxes-alist '(displayname . 15))

    (eudc-protocol-set
     'eudc-inline-expansion-format
     '("%s <%s> {%s %s}" displayname email company department)
     'ldap)
    (eudc-protocol-set
     'eudc-inline-query-format
     '((cn) (mail) (cn cn) (cn cn cn) (sn) (uid) (givenname) (givenname name) (name))
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


(provide 'apps/eudc)
;;; eudc.el ends here
