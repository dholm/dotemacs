;;; popup --- visual popups for Emacs
;;; Commentary:
;;; Code:

(defun user/popup-init ()
  "Initialize popup."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(popup-face ((t (:foreground ,solarized-fg :background ,solarized-hl))))
         '(popup-isearch-match ((t (:foreground ,solarized-bg :background ,yellow))))
         '(popup-menu-face ((t (:foreground ,solarized-fg :background ,solarized-hl))))
         '(popup-menu-mouse-face ((t (:foreground ,solarized-fg :background ,blue))))
         '(popup-menu-selection-face ((t (:foreground ,solarized-bg :background ,magenta))))
         '(popup-scroll-bar-background-face ((t (:background ,solarized-comment))))
         '(popup-scroll-bar-foreground-face ((t (:background ,solarized-emph))))
         '(popup-tip-face ((t (:foreground ,solarized-fg :background ,solarized-hl)))))))))

(require-package '(:name popup :after (user/popup-init)))


(provide 'utilities/popup)
;;; popup.el ends here
