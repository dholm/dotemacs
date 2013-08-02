;;; ace-jump-mode --- quick buffer navigation
;;; Commentary:
;;; Code:

(defun user/ace-jump-mode-init ()
  "Initialize ace jump mode."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(ace-jump-face-background ((t (:foreground ,solarized-comment :background ,solarized-bg
                                                     :inverse-video nil))))
         '(ace-jump-face-foreground ((t (:foreground ,red :background ,solarized-bg
                                                     :inverse-video nil :weight bold))))))))

  (define-key user/navigation-map (kbd "SPC") 'ace-jump-mode))

(require-package '(:name ace-jump-mode
			 :after (user/ace-jump-mode-init)))


(provide 'ux/ace-jump-mode)
;;; ace-jump-mode.el ends here
