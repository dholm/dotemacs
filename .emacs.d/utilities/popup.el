;;; popup --- visual popups for Emacs
;;; Commentary:
;;; Code:

(defun dholm/popup-init ()
  ;;; (Faces) ;;;
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized
       '(popup-face ((t (:background ,base02 :foreground ,base0))))
       '(popup-isearch-match ((t (:background ,yellow :foreground ,base03))))
       '(popup-menu-face ((t (:background ,base02 :foreground ,base0))))
       '(popup-menu-mouse-face ((t (:background ,blue :foreground ,base0))))
       '(popup-menu-selection-face ((t (:background ,magenta :foreground ,base03))))
       '(popup-scroll-bar-background-face ((t (:background ,base01))))
       '(popup-scroll-bar-foreground-face ((t (:background ,base1))))
       '(popup-tip-face ((t (:background ,base02 :foreground ,base0))))))))

(require-package '(:name popup :after (dholm/popup-init)))


(provide 'utilities/popup)
;;; popup.el ends here
