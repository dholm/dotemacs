;;; undo.el --- Configure Emacs undo
;;; Commentary:
;;; Code:

(defun user/undo-tree-init ()
  "Initialize undo-tree."
  (require 'undo-tree)

  (global-undo-tree-mode t)
  (after-load 'diminish
    (diminish 'undo-tree-mode))

  (define-key user/code-map (kbd "u") 'undo-tree-visualize))


(defun user/undo-init ()
  "Initialize Emacs undo."
  (require-package '(:name undo-tree :after (user/undo-tree-init))))

(user/undo-init)


(provide 'ux/undo)
;;; undo.el ends here
