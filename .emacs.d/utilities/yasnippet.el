(require-package (:name yasnippet :after (dholm/yasnippet-init)))


;; Look for snippets in data directory
(setq yas-snippet-dir (path-join *user-data-directory* "snippets"))


(defun dholm/yasnippet-init ()
  ;; Enables yasnippet globally
  (yas-global-mode)
  (diminish 'yas-minor-mode))


(provide 'utilities/yasnippet)
