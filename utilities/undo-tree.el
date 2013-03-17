;; Visualize undo history as a tree structure

(push "~/.emacs.d/utilities/undo-tree" load-path)
(require 'undo-tree)
(when (featurep 'undo-tree)
  (global-undo-tree-mode))
