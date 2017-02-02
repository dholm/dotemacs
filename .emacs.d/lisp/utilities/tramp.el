;;; tramp --- remote file access
;;; Commentary:
;;; Code:

(defun user--tramp-config ()
  "Initialize tramp."
  (validate-setq
   ;; Auto save storage.
   tramp-auto-save-directory (path-join *user-auto-save-directory* "tramp")
   ;; Default file transfer method.
   tramp-default-method "ssh"
   ;; Cache passwords.
   password-cache t
   password-cache-expiry 1000
   ;; SSH is properly configured to share connections.
   tramp-use-ssh-controlmaster-options nil
   ;; Skip looking for dir-local on remote system to speed up tramp.
   enable-remote-dir-locals nil)

  ;; Load SSH configuration
  (after-load 'tramp
    (tramp-set-completion-function
     "ssh" (mapcar
            (lambda (x) (list 'tramp-parse-sconfig x))
            (-remove
             (lambda (x) (not (file-exists-p x)))
             `(,(path-join "/" "etc" "ssh_config")
               ,(path-join "/" "etc" "ssh" "ssh_config")
               ,(path-join *user-home-directory* ".ssh" "config")))))

    ;; Preserve PATH on remote host.
    (setq tramp-remote-path (delete 'tramp-default-remote-path tramp-remote-path))
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))

(use-package tramp
  :ensure t
  :after dash
  :config (user--tramp-config))

(use-package tramp-cache
  :after tramp
  :config
  (validate-setq
   ;; Persistency files.
   tramp-persistency-file-name (path-join *user-cache-directory* "tramp")))


(provide 'utilities/tramp)
;;; tramp.el ends here
