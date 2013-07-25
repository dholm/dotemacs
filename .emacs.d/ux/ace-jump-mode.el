;;; ace-jump-mode --- quick buffer navigation
;;; Commentary:
;;; Code:

(defun dholm/ace-jump-mode-init ()
  ;;; (Faces) ;;;
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized
       '(ace-jump-face-background ((t (:foreground ,base01 ,@back))))
       '(ace-jump-face-foreground ((t (:foreground ,red ,@back ,@fmt-bold)))))))

  (global-set-key (kbd "C-c n SPC") 'ace-jump-mode))

(require-package '(:name ace-jump-mode
			 :depends (solarized-theme)
			 :after (dholm/ace-jump-mode-init)))


(provide 'ux/ace-jump-mode)
;;; ace-jump-mode.el ends here
