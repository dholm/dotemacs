;;; .emacs.local.el --- local Emacs configuration
;;; Commentary:
;;; Code:

(after-load 'magit
  ;; Set up root directory where magit will find Git repositories.
  (add-to-list 'magit-repo-dirs (path-join *user-home-directory* "Projects")))

(after-load 'ede
  (when (require 'ede/cpp-root nil t)
    ;; Generic EDE project example.
    (when (file-exists-p "/path/to/project/root/file")
      (ede-cpp-root-project "Project"
                            :name "Project Name"
                            :file "/path/to/project/root/file"
                            :local-variables '((fill-column . 80)
                                               (whitespace-line-column . 80))
                            :include-path '("libc" "libd")
                            :system-include-path '()
                            :spp-table '(("DEBUG" . ""))
                            :compile-command "nice make -j"))))

(after-load 'elfeed
  (setq elfeed-feeds
        ;; Set up web feeds to read using elfeed.
        '(("http://dholm.com/feed/" blog personal)
          ("http://nullprogram.com/feed/" blog programming))))


;; Keymap override.
(user/global-keymap-overlay
 '((:basic . ((:open-file-context . "C-x f")))
   (:nav . ((:go-back . "C-x n")
            (:follow-symbol . "C-x m")))))


;; Email setup.
(after-load 'wl
  ;; Wanderlust
  (user/wanderlust-set-gmail-user "John Doe" "john.doe")
  ;;; (Bindings) ;;;
  (define-key wl-summary-mode-map (kbd "b a") ;; => Archive
    '(lambda ()
       (interactive)
       (user/wanderlust-summary-refile "%Archive"))))

(after-load 'gnus
  ;; Gnus
  (user/gnus-set-gmail-user "John Doe" "john.doe")

  ;; S/MIME
  (add-to-list
   'smime-keys
   `(,email-address
     ,(path-join *user-home-directory* ".ssl" "certificate.pem") nil))
  (setq
   smime-ldap-host-list '("ldap.host.com")
   jl-smime-permit-ldap "@\\(.+\\.\\)?company\\.com$"))


;; Directory services.
(after-load 'ldap
  (setq-default
   ldap-host-parameters-alist
   '(("ldap.host.com" base "dc=company,dc=com"
      binddn "uid=jdoe,ou=users,o=company"))))
(after-load 'eudc
  (eudc-set-server "ldap.host.com" 'ldap t)
  (add-to-list 'eudc-server-hotlist '("ldap.host.com" . ldap)))


(provide '.emacs.local)
;;; .emacs.local.el ends here
