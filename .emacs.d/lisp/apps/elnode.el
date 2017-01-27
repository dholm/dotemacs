;;; elnode.el --- Emacs event driven web framework.
;;; Commentary:
;;; Code:

(defun user--elnode-config ()
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

(req-package elnode
  :config (user--elnode-config))
(req-package elnode-org
  :loader :el-get
  :config (user--elnode-config))


(provide 'apps/elnode)
;;; elnode.el ends here
