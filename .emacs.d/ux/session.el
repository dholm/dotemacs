;;; (Initialization) ;;;
(require-package (:name session
                        :before (dholm/session-before-init)
                        :after (dholm/session-init)))


(defun dholm/session-before-init ()
  (custom-set-variables '(session-use-package t nil (session))))


(defun dholm/session-init ()
  (setq-default
   session-save-file (path-join *user-cache-directory* "session")
   desktop-globals-to-save
   (append '((extended-command-history . 30)
             (file-name-history        . 100)
             (ido-last-directory-list  . 100)
             (ido-work-directory-list  . 100)
             (ido-work-file-list       . 100)
             (grep-history             . 30)
             (compile-history          . 30)
             (minibuffer-history       . 50)
             (query-replace-history    . 60)
             (read-expression-history  . 60)
             (regexp-history           . 60)
             (regexp-search-ring       . 20)
             (search-ring              . 20)
             (comint-input-ring        . 50)
             (shell-command-history    . 50)
             desktop-missing-file-warning
             tags-file-name
             register-alist))))


(add-hook 'after-init-hook 'session-initialize)


(provide 'ux/session)
