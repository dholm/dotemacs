;;; auth-source.el --- Configure Emacs authentication sources
;;; Commentary:
;;; Code:

(defun user/auth-source-init ()
  "Initialize auth-source."
  (setq-default
   auth-sources
   `((:source ,(path-join *user-data-directory* "authinfo.gpg")
              :host t :protocol t)
     (:source ,(path-join *user-data-directory* "authinfo")
              :host t :protocol t)))

  (dolist (source auth-sources)
    (let ((file (plist-get source :source)))
      (when (file-exists-p file)
        (set-file-modes file #o0600)))))

(user/auth-source-init)


(provide 'utilities/auth-source)
;;; auth-source.el ends here
