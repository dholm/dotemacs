;;; rainbow-delimiters --- match up parenthesis using colors
;;; Commentary:
;;; Code:

(defun user/rainbow-delimiters-init ()
  "Initialize rainbow delimiters."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(rainbow-delimiters-depth-1-face ((t (:foreground ,cyan))))
         '(rainbow-delimiters-depth-2-face ((t (:foreground ,yellow))))
         '(rainbow-delimiters-depth-3-face ((t (:foreground ,blue))))
         '(rainbow-delimiters-depth-4-face ((t (:foreground ,orange))))
         '(rainbow-delimiters-depth-5-face ((t (:foreground ,green))))
         '(rainbow-delimiters-depth-6-face ((t (:foreground ,yellow))))
         '(rainbow-delimiters-depth-7-face ((t (:foreground ,blue))))
         '(rainbow-delimiters-depth-8-face ((t (:foreground ,orange))))
         '(rainbow-delimiters-depth-9-face ((t (:foreground ,green))))
         '(rainbow-delimiters-depth-10-face ((t (:foreground ,yellow))))
         '(rainbow-delimiters-depth-11-face ((t (:foreground ,blue))))
         '(rainbow-delimiters-depth-12-face ((t (:foreground ,orange))))
         '(rainbow-delimiters-unmatched-face
          ((t (:foreground ,solarized-fg :background ,solarized-bg :inverse-video t)))))))))

(require-package '(:name rainbow-delimiters :after (user/rainbow-delimiters-init)))


(provide 'ux/rainbow-delimiters)
;;; rainbow-delimiters.el ends here
