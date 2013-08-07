;;; tramp --- remote file access
;;; Commentary:
;;; Code:

(defun user/tramp-init ()
  "Initialize tramp."
  (setq-default
   tramp-default-method "ssh"
   tramp-persistency-file-name (path-join *user-cache-directory* "tramp"))

  ;; Load SSH configuration
  (tramp-set-completion-function "ssh"
                                 `((tramp-parse-sconfig
                                    "/etc/ssh/ssh_config")
                                   (tramp-parse-sconfig
                                    ,(path-join *user-home-directory* ".ssh" "config")))))

(after-load 'tramp
  (user/tramp-init))


(provide 'utilities/tramp)
;;; tramp.el ends here
