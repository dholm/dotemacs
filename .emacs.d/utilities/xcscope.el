;;; xcscope --- cscope integration
;;; Commentary:
;;; Code:

(require-package '(:name xcscope
			 :type github
			 :pkgname "dholm/xcscope"
			 :features xcscope
			 :prepare (progn
				    (setq cscope-indexing-script
					  (expand-file-name
					   (concat (el-get-package-directory "xcscope") "cscope-indexer"))))))


(provide 'utilities/xcscope)
;;; xcscope.el ends here
