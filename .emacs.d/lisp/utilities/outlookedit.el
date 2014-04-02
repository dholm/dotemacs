;;; outlookedit.el --- outlook integration
;;; Commentary:
;;; Code:

(when (eq system-type 'windows-nt)
  (require-package '(:name outlookedit :features (outlookedit))))


(provide 'utilities/outlookedit)
;;; outlookedit.el ends here
