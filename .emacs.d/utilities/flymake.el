;;; flymake --- build on the fly
;;; Commentary:
;;; Code:

(defun dholm/flymake-init ()
  "Initialize flymake."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(flymake-errline
           ((,'((supports :underline (:style wave)))
             (:underline (:style wave :color ,red) :inherit unspecified
                         :foreground ,red-hc :background ,red-lc))
            (t (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
         '(flymake-infoline
           ((,'((supports :underline (:style wave)))
             (:underline (:style wave :color ,green) :inherit unspecified
                         :foreground ,green-hc :background ,green-lc))
            (t (:foreground ,green-hc :background ,green-lc :weight bold :underline t))))
         '(flymake-warnline
           ((,'((supports :underline (:style wave)))
             (:underline (:style wave :color ,yellow) :inherit unspecified
                         :foreground ,yellow-hc :background ,yellow-lc))
            (t (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t)))))))))

(require-package '(:name flymake-cursor :after (dholm/flymake-init)))


(provide 'utilities/flymake)
;;; flymake.el ends here
