;;; outlookedit.el --- outlook integration
;;; Commentary:
;;; Code:

(if (eq system-type 'windows-nt)
    (require-package '(:name outlookedit
                             :type github
                             :pkgname "dholm/outlookedit"
                             :features (outlookedit))))


(provide 'utilities/outlookedit)
;;; outlookedit.el ends here
