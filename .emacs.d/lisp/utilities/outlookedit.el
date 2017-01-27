;;; outlookedit.el --- outlook integration
;;; Commentary:
;;; Code:

(when (eq system-type 'windows-nt)
  (req-package outlookedit
    :loader :el-get))


(provide 'utilities/outlookedit)
;;; outlookedit.el ends here
