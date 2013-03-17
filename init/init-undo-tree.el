;; Visualize undo history as a tree structure

(require 'undo-tree)
(when (featurep 'undo-tree)
  (global-undo-tree-mode))
