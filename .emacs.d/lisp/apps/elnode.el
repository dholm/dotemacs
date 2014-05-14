;;; elnode.el --- Emacs event driven web framework.
;;; Commentary:
;;; Code:

(defun user/elnode-init ()
  "Initialize Elnode."
  (setq-default
   ;; Do not start server automatically.
   elnode-do-init nil
   ;; Don't spam *Messages* with error logs.
   elnode-error-log-to-messages nil
   ;; Log store.
   elnode-log-files-directory (path-join *user-cache-directory* "elnode"))

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :elnode 'elnode-start))

(require-package '(:name elnode :after (user/elnode-init)))
(require-package '(:name elnode-org :after (user/elnode-init)))


(provide 'apps/elnode)
;;; elnode.el ends here
