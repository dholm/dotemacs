;;; tramp --- remote file access
;;; Commentary:
;;; Code:

(defun user/tramp-init ()
  "Initialize tramp."
  (setq-default
   ;; Default file transfer method.
   tramp-default-method "scp"
   ;; Cache passwords.
   password-cache t
   password-cache-expiry 1000
   ;; Persistency files.
   tramp-persistency-file-name (path-join *user-cache-directory* "tramp")
   ;; Auto save storage.
   tramp-auto-save-directory (path-join *user-auto-save-directory* "tramp"))

  ;; Load SSH configuration
  (after-load 'tramp
    (tramp-set-completion-function
     "ssh" (-map
            (lambda (x) (list 'tramp-parse-sconfig x))
            (-remove
             (lambda (x) (not (file-exists-p x)))
             `(,(path-join "/" "etc" "ssh_config")
               ,(path-join "/" "etc" "ssh" "ssh_config")
               ,(path-join *user-home-directory* ".ssh" "config")))))))

(require-package '(:name tramp :after (user/tramp-init)))


(provide 'utilities/tramp)
;;; tramp.el ends here
