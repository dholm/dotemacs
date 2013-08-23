;;; tabkey2.el --- smart tab key
;;; Commentary:
;;; Code:

(require-package '(:name tabkey2
                         :type http
                         :url "http://marmalade-repo.org/packages/tabkey2-1.40.el"
                         :build '(("mv" "tabkey2-1.40.el" "tabkey2.el"))
                         :compile "tabkey2.el"
                         :autoloads "tabkey2"))


(provide 'ux/tabkey2)
;;; tabkey2.el ends here
