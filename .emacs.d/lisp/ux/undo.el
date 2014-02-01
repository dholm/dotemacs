;;; undo.el --- Configure Emacs undo
;;; Commentary:
;;; Code:

(defun user/undo-tree-init ()
  "Initialize undo-tree."
  (require 'undo-tree)

  (global-undo-tree-mode t)
  (after-load 'diminish
    (diminish 'undo-tree-mode))

  (global-set-key [remap undo] 'undo-tree-undo)
  (user/bind-key-global :basic :redo 'undo-tree-redo)
  (user/bind-key-global :util :undo-tree 'undo-tree-visualize))


(defun user/undo-init ()
  "Initialize Emacs undo."
  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :undo 'undo)

  ;;; (Packages) ;;;
  (require-package '(:name undo-tree :after (user/undo-tree-init))))

(user/undo-init)


(provide 'ux/undo)
;;; undo.el ends here
