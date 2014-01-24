;;; ctable --- Table component for Emacs
;;; Commentary:
;;; Code:

(defun user/ctable-init ()
  "Initialize ctable."
  ;; Register autoload for lib/benchmark
  (autoload 'make-ctbl:cmodel "ctable"))

(require-package '(:name ctable :after (user/ctable-init)))


(provide 'utilities/ctable)
;;; ctable.el ends here
