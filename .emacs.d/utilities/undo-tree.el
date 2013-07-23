;;; undo-tree --- undo history in a tree structure
;;; Commentary:
;;; Code:

(require-package '(:name undo-tree :after (dholm/undo-tree-init)))

(defun dholm/undo-tree-init ()
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode))


(provide 'utilities/undo-tree)
;;; undo-tree.el ends here
