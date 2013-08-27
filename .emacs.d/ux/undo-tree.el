;;; undo-tree.el --- undo history in a tree structure
;;; Commentary:
;;; Code:

(defun user/undo-tree-init ()
  "Initialize undo-tree."
  (require 'undo-tree)

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(undo-tree-visualizer-default-face ((t (:foreground ,solarized-comment :background ,solarized-bg))))
         '(undo-tree-visualizer-unmodified-face ((t (:foreground ,green))))
         '(undo-tree-visualizer-current-face ((t (:foreground ,blue :inverse-video t))))
         '(undo-tree-visualizer-active-branch-face ((t (:foreground ,solarized-emph :background ,solarized-bg
                                                                    :weight bold))))
         '(undo-tree-visualizer-register-face ((t (:foreground ,yellow))))))))

  (global-undo-tree-mode t)
  (after-load 'diminish
    (diminish 'undo-tree-mode))

  (define-key user/code-map (kbd "u") 'undo-tree-visualize))

(require-package '(:name undo-tree
                         :features (undo-tree)
                         :type git
                         :url "http://www.dr-qubit.org/git/undo-tree.git/"
                         :after (user/undo-tree-init)))


(provide 'ux/undo-tree)
;;; undo-tree.el ends here