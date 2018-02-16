;;; smtpmail.el --- Emacs SMTP -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-smtpmail-data-directory*
  (path-join *user-data-directory* "smtpmail")
  "Path to user's smtpmail data store.")

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
  :init
  ;; Create directory for send queue.
  (let ((smtpmail-queue-dir
         (path-join *user-smtpmail-data-directory* "send-queue")))
    (make-directory smtpmail-queue-dir t)
    (set-file-modes smtpmail-queue-dir #o0700))
  :config
  (validate-setq
   ;; Location of send queue.
   smtpmail-queue-dir
   (path-join *user-smtpmail-data-directory* "send-queue")
   ;; Use smtpmail as the default method of sending email.
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'message-smtpmail-send-it)

  (use-package smtpmail-multi
    :config
    (validate-setq
     ;; Use smtpmail-multi as the default method of sending email.
     send-mail-function 'smtpmail-send-it
     message-send-mail-function 'smtpmail-send-it))

  (use-package smtpmail-async
    :after async
    :ensure nil
    :config
    (validate-setq
     ;; Default to asynchronous mode.
     send-mail-function 'async-smtpmail-send-it
     message-send-mail-function 'async-smtpmail-send-it)))


(provide 'utilities/smtpmail)
;;; smtpmail.el ends here
