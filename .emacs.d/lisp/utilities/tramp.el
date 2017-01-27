;;; tramp --- remote file access
;;; Commentary:
;;; Code:

(defun user/tramp-before-init ()
  "Initialization before loading tramp."
  (setq-default
   ;; Persistency files.
   tramp-persistency-file-name (path-join *user-cache-directory* "tramp")
   ;; Auto save storage.
   tramp-auto-save-directory (path-join *user-auto-save-directory* "tramp")))


(defun user/tramp-init ()
  "Initialize tramp."
  (setq-default
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
     "ssh" (-map
            (lambda (x) (list 'tramp-parse-sconfig x))
            (-remove
             (lambda (x) (not (file-exists-p x)))
             `(,(path-join "/" "etc" "ssh_config")
               ,(path-join "/" "etc" "ssh" "ssh_config")
               ,(path-join *user-home-directory* ".ssh" "config")))))

    ;; Preserve PATH on remote host.
    (setq tramp-remote-path (delete 'tramp-default-remote-path tramp-remote-path))
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))

(req-package tramp
  :require dash
  :init (user/tramp-before-init)
  :config (user/tramp-init))


(provide 'utilities/tramp)
;;; tramp.el ends here
