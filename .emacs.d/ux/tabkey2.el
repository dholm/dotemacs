;;; tabkey2.el --- smart tab key
;;; Commentary:
;;; Code:

(defun user/tabkey2-init ()
  "Initialize tabkey2."
  ;; Just load it since it is used by prog-mode
  (require 'tabkey2))

(require-package '(:name tabkey2
                         :type http
                         :url "http://marmalade-repo.org/packages/tabkey2-1.40.el"
                         :build '(("mv" "tabkey2-1.40.el" "tabkey2.el"))
                         :compile "tabkey2.el"
                         :autoloads "tabkey2"
                         :after (user/tabkey2-init)))


(provide 'ux/tabkey2)
;;; tabkey2.el ends here
