;;; .emacs.local.el --- local Emacs configuration
;;; Commentary:
;;; Code:

(with-eval-after-load 'magit
  ;; Set up root directory where magit will find Git repositories.
  (add-to-list 'magit-repo-dirs (path-join *user-home-directory* "Projects")))

(with-eval-after-load 'ede
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

(with-eval-after-load 'elfeed
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
(with-eval-after-load 'wl
  ;; Wanderlust
  (user/wanderlust-set-gmail-user "John Doe" "john.doe")
  ;;; (Bindings) ;;;
  (define-key wl-summary-mode-map (kbd "b a") ;; => Archive
    '(lambda ()
       (interactive)
       (user/wanderlust-summary-refile "%Archive"))))

(with-eval-after-load 'gnus
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
(with-eval-after-load 'ldap
  (setq-default
   ldap-host-parameters-alist
   '(("ldap.host.com" base "dc=company,dc=com"
      binddn "uid=jdoe,ou=users,o=company"))))
(with-eval-after-load 'eudc
  (eudc-set-server "ldap.host.com" 'ldap t)
  (add-to-list 'eudc-server-hotlist '("ldap.host.com" . ldap)))


;; Weather info.
(setq-default
 wttrin-default-cities '("San Jose" "Stockholm"))


;; Set up cryptocurrency ticker.
(with-eval-after-load 'coin-ticker
  (validate-setq
   coin-ticker-price-convert "SEK"
   coin-ticker-price-symbol ""
   coin-ticker-syms '("BTC" "ETH" "DCR")))


(provide '.emacs.local)
;;; .emacs.local.el ends here
