;;; flyspell --- spell checking on the fly
;;; Commentary:
;;; Code:

(defun dholm/deferred-flyspell-init ()
  ;;; (Faces) ;;;
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized
       ;; Overrides for flyspell piggyback on deferred-flyspell due to the
       ;; dependency chain with solarized-theme.
       ,@(if window-system
	     `('(flyspell-duplicate ((t (:foreground ,yellow :underline (:style wave :color ,yellow) :inherit unspecified))))
	       '(flyspell-incorrect ((t (:foreground ,red :underline (:style wave :color ,red) :inherit unspecified)))))
	   `('(flyspell-duplicate ((t (:foreground ,yellow :weight bold :underline t))))
	     '(flyspell-incorrect ((t (:foreground ,red :weight bold :underline t))))))))))

(require-package '(:name deferred-flyspell :after (dholm/deferred-flyspell-init)))


(provide 'utilities/flyspell)
;;; flyspell.el ends here
