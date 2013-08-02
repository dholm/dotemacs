;;; powerline --- powerline mode-line
;;; Commentary:
;;; Code:

(defun user/powerline-init ()
  "Initialize powerline."
  (require 'powerline)

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(mode-line ((t (:foreground ,solarized-fg :background ,solarized-comment))))
         '(mode-line-buffer-id ((t (:foreground ,base2 :background ,blue))))
         '(mode-line-inactive ((t (:foreground ,solarized-fg :background ,solarized-comment))))
         '(powerline-active1 ((t (:foreground ,solarized-comment :background ,solarized-hl))))
         '(powerline-active2 ((t (:foreground ,orange :background ,solarized-hl))))
         '(powerline-inactive1 ((t (:foreground ,solarized-fg :background ,solarized-hl))))
         '(powerline-inactive2 ((t (:foreground ,solarized-fg :background ,solarized-comment))))))))

  ;;; (Theme) ;;;
  (setq-default powerline-arrow-shape 'slant)
  (powerline-default-theme))

(require-package '(:name powerline
			 :type github
			 :pkgname "milkypostman/powerline"
			 :after (user/powerline-init)
			 :prepare (autoload 'powerline-default-theme "powerline" nil t)))


(provide 'ux/powerline)
;;; powerline.el ends here
