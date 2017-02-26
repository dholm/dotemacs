;;; smtpmail.el --- Emacs SMTP
;;; Commentary:
;;; Code:

(defun user/smtpmail-set-gmail-user (fullname username)
  "Configure smtpmail to use \"FULLNAME\" <USERNAME@gmail.com>."
  (let ((email-address (concat username "@gmail.com")))
    (if (feature-p 'smtpmail-multi)
        (with-feature 'smtpmail-multi
          (add-to-list 'smtpmail-multi-accounts
                       `(,email-address
                         (,email-address "smtp.gmail.com" 587
                                         ,(concat fullname " <" email-address ">")
                                         starttls nil nil "gmail.com"))))
      (setq
       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
       smtpmail-auth-credentials `(("smtp.gmail.com" 587 ,email-address nil))
       smtpmail-default-smtp-server "smtp.gmail.com"
       smtpmail-smtp-server "smtp.gmail.com"
       smtpmail-smtp-service 587
       smtpmail-local-domain "gmail.com"
       mail-host-address "gmail.com"))))

(use-package smtpmail
  :defer
  :config
  (validate-setq
   ;; Use smtpmail as the default method of sending email.
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it
   ;; Queue mail when offline.
   smtpmail-queue-mail t)

  (use-package smtpmail-multi
    :config
    (validate-setq
     ;; Use smtpmail-multi as the default method of sending email.
     send-mail-function 'smtpmail-send-it
     message-send-mail-function 'smtpmail-send-it)))


(provide 'utilities/smtpmail)
;;; smtpmail.el ends here
