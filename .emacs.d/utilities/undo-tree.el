(require-package (:name undo-tree :after (dholm/undo-tree-init)))

(defun dholm/undo-tree-init ()
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode))


(provide 'utilities/undo-tree)
