;;; auth-source.el --- Configure Emacs authentication sources
;;; Commentary:
;;; Code:

(use-package auth-source
  :ensure nil
  :defer t
  :config
  (validate-setq
   auth-sources
   `(,(path-join *user-data-directory* "authinfo.gpg")

     ,(path-join *user-data-directory* "authinfo")))

  (dolist (source auth-sources)
    (when (file-exists-p source)
      (set-file-modes source #o0600)))

  (when (eq system-type 'darwin)
    (add-many-to-list 'auth-sources
                      'macos-keychain-internet
                      'macos-keychain-generic)))


(provide 'utilities/auth-source)
;;; auth-source.el ends here
