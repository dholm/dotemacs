;;; undo-tree --- undo history in a tree structure
;;; Commentary:
;;; Code:

(require-package '(:name undo-tree :after (dholm/undo-tree-init)))

(defun dholm/undo-tree-init ()
  ;;; (Faces) ;;;
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized
       '(undo-tree-visualizer-default-face ((t (:foreground ,base01 ,@back))))
       '(undo-tree-visualizer-unmodified-face ((t (:foreground ,green))))
       '(undo-tree-visualizer-current-face ((t (:foreground ,blue :inverse-video t))))
       '(undo-tree-visualizer-active-branch-face ((t (:foreground ,base1 ,@back :weight bold))))
       '(undo-tree-visualizer-register-face ((t (:foreground ,yellow)))))))

  (global-undo-tree-mode t)
  (diminish 'undo-tree-mode))


(provide 'utilities/undo-tree)
;;; undo-tree.el ends here
