;;; page-break-lines --- prettify page breaks
;;; Commentary:
;;; Code:

(defun dholm/page-break-lines-init ()
  "Initialize page break lines."
  (require 'page-break-lines)
  (global-page-break-lines-mode)
  (diminish 'page-break-lines-mode))

(require-package '(:name page-break-lines
			 :type github
			 :pkgname "purcell/page-break-lines"
			 :after (dholm/page-break-lines-init)))


(provide 'ux/page-break-lines)
;;; page-break-lines.el ends here
