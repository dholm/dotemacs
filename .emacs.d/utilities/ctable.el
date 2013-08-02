;;; ctable --- Table component for Emacs
;;; Commentary:
;;; Code:

(defun user/ctable-init ()
  "Initialize ctable."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(ctbl:face-cell-select ((t (:foreground ,solarized-emph :background ,solarized-hl
                                                  :underline ,solarized-emph :weight bold))))
         '(ctbl:face-continue-bar ((t (:foreground ,yellow :background ,solarized-hl))))
         '(ctbl:face-row-select ((t (:foreground ,solarized-fg :background ,solarized-hl
                                                 :underline t)))))))))

(require-package '(:name ctable :after (user/ctable-init)))


(provide 'utilities/ctable)
;;; ctable.el ends here
