;;; yasnippet --- snippet support
;;; Commentary:
;;; Code:

(defun dholm/yasnippet-init ()
  "Initialize yasnippet."
  ;; Look for snippets in data directory
  (setq-default
   yas-snippet-dirs (path-join *user-data-directory* "snippets"))
  (make-directory yas-snippet-dirs t)

  ;; Enables yasnippet globally
  (yas-global-mode)
  (after-load 'diminish
    (diminish 'yas-minor-mode)))

(require-package '(:name yasnippet :after (dholm/yasnippet-init)))


(provide 'utilities/yasnippet)
;;; yasnippet.el ends here
