;;; flymake --- build on the fly
;;; Commentary:
;;; Code:

(defun dholm/flymake-init ()
  ;;; (Faces) ;;;
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized
       ,@(if window-system
	     `('(flymake-errline ((t (:foreground ,red :weight bold :underline (:style wave :color ,red) :inherit unspecified))))
	       '(flymake-infoline ((t (:foreground ,green :underline (:style wave :color ,green) :inherit unspecified))))
	       '(flymake-warnline ((t (:foreground ,yellow :weight bold :underline (:style wave :color ,yellow) :inherit unspecified)))))
	   `('(flymake-errline ((t (:foreground ,red :weight bold :underline t))))
	     '(flymake-infoline ((t (:foreground ,green))))
	     '(flymake-warnline ((t (:foreground ,yellow :weight bold :underline t))))))))))


(require-package '(:name flymake-cursor :after (dholm/flymake-init)))


(provide 'utilities/flymake)
;;; flymake.el ends here
