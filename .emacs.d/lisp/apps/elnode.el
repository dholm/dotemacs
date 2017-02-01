;;; elnode.el --- Emacs event driven web framework.
;;; Commentary:
;;; Code:

(defun user--elnode-config ()
  "Initialize Elnode."
  (validate-setq
   ;; Do not start server automatically.
   elnode-do-init nil
   ;; Don't spam *Messages* with error logs.
   elnode-error-log-to-messages nil
   ;; Log store.
   elnode-log-files-directory (path-join *user-cache-directory* "elnode"))

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :elnode 'elnode-start))

(use-package elnode
  :defer t
  :config (user--elnode-config))
(require-package '(:name elnode-org))


(provide 'apps/elnode)
;;; elnode.el ends here
