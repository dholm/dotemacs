;;; elnode.el --- Emacs event driven web framework.
;;; Commentary:
;;; Code:

(use-package elnode
  :commands elnode-start
  :init
  (user/bind-key-global :apps :elnode 'elnode-start)
  :config
  (validate-setq
   ;; Do not start server automatically.
   elnode-do-init nil
   ;; Don't spam *Messages* with error logs.
   elnode-error-log-to-messages nil
   ;; Log store.
   elnode-log-files-directory (path-join *user-cache-directory* "elnode"))

  (use-package elnode-org
    :quelpa (elnode-org
             :fetcher github
             :repo "nicferrier/elnode-org")
    :defer t
    :requires elnode org))


(provide 'apps/elnode)
;;; elnode.el ends here
