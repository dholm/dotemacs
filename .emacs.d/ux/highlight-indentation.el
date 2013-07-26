;;; highlight-indentation --- draws guides for indentation levels
;;; Commentary:
;;; Code:

(defun dholm/highlight-indentation-init ()
  "Initialize highlight indentation."
  ;;; (Faces) ;;;
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized
       '(highlight-indentation-face ((t (:background ,base02))))
       '(highlight-indentation-current-column-face ((t (:background ,base02))))))))

(require-package '(:name highlight-indentation
                         :depends (solarized-theme)
                         :after (dholm/highlight-indentation-init)))


(provide 'ux/highlight-indentation)
;;; highlight-indentation.el ends here
