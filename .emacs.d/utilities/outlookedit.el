;;; outlookedit --- outlook integration
;;; Commentary:
;;; Code:

(push (path-join user-emacs-directory "utilities" "outlookedit") load-path)

(if (eq system-type 'windows-nt)
    (require 'outlookedit))


(provide 'utilities/outlookedit)
;;; outlookedit.el ends here
