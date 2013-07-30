;;; undo-tree --- undo history in a tree structure
;;; Commentary:
;;; Code:

(defun dholm/undo-tree-init ()
  "Initialize undo-tree."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(undo-tree-visualizer-default-face ((t (:foreground ,base01 ,@back))))
         '(undo-tree-visualizer-unmodified-face ((t (:foreground ,green))))
         '(undo-tree-visualizer-current-face ((t (:foreground ,blue :inverse-video t))))
         '(undo-tree-visualizer-active-branch-face ((t (:foreground ,base1 ,@back :weight bold))))
         '(undo-tree-visualizer-register-face ((t (:foreground ,yellow))))))))

  (global-undo-tree-mode t)
  (after-load 'diminish
    (diminish 'undo-tree-mode))

  (define-key dholm/utilities-map (kbd "u") 'undo-tree-visualize))

(require-package '(:name undo-tree
                         :features (undo-tree)
                         :type git
                         :url "http://www.dr-qubit.org/git/undo-tree.git/"
                         :after (dholm/undo-tree-init)))


(provide 'utilities/undo-tree)
;;; undo-tree.el ends here
