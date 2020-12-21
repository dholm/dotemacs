;;; notmuch.el --- sets up notmuch -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package notmuch
  :if (executable-find "notmuch")
  :config
  (use-package helm-notmuch)
  (use-package notmuch-bookmarks
    :config
    (notmuch-bookmarks-mode))
  (use-package notmuch-labeler))


(provide 'apps/notmuch)
;;; notmuch.el ends here
