;;; xcscope --- cscope integration
;;; Commentary:
;;; Code:

(defun dholm/xcscope-init ()
  ;;; (Faces) ;;;
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized
       '(cscope-file-face ((t (:foreground ,green :weight bold))))
       '(cscope-function-face ((t (:foreground ,blue))))
       '(cscope-line-number-face ((t (:foreground ,yellow))))
       '(cscope-line-face ((t (:foreground ,base0))))
       '(cscope-mouse-face ((t (:foreground ,base0 :background ,blue))))))))


(require-package '(:name xcscope
			 :type github
			 :pkgname "dholm/xcscope"
			 :features xcscope
			 :after (dholm/xcscope-init)
			 :prepare (progn
				    (setq cscope-indexing-script
					  (expand-file-name
					   (concat (el-get-package-directory "xcscope") "cscope-indexer"))))))


(provide 'utilities/xcscope)
;;; xcscope.el ends here
