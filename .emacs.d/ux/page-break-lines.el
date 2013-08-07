;;; page-break-lines --- prettify page breaks
;;; Commentary:
;;; Code:

(defun user/page-break-lines-init ()
  "Initialize page break lines."
  (require 'page-break-lines)
  (global-page-break-lines-mode)
  (after-load 'diminish
    (diminish 'page-break-lines-mode)))

(require-package '(:name page-break-lines :after (user/page-break-lines-init)))


(provide 'ux/page-break-lines)
;;; page-break-lines.el ends here
