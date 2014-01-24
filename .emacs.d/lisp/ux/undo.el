;;; undo.el --- Configure Emacs undo
;;; Commentary:
;;; Code:

(defun user/undo-init ()
  "Initialize Emacs undo."
  (require-package '(:name undo-tree :after (user/undo-tree-init))))


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


(user/undo-init)


(provide 'ux/undo)
;;; undo.el ends here
