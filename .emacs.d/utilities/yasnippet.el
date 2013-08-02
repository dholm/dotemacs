;;; yasnippet --- snippet support
;;; Commentary:
;;; Code:

(defun user/yasnippet-init ()
  "Initialize yasnippet."
  ;; Look for snippets in data directory
  (setq-default
   yas-snippet-dirs (path-join *user-data-directory* "snippets"))
  (make-directory yas-snippet-dirs t)

  ;; Add yasnippet as auto-complete source
  (after-load 'auto-complete
    (add-to-list 'ac-sources 'ac-source-yasnippet))

  ;; Enables yasnippet globally
  (yas-global-mode)
  (after-load 'diminish
    (diminish 'yas-minor-mode)))

(require-package '(:name yasnippet :after (user/yasnippet-init)))


(provide 'utilities/yasnippet)
;;; yasnippet.el ends here
