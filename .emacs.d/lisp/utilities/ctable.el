;;; ctable --- Table component for Emacs
;;; Commentary:
;;; Code:

(defun user--ctable-config ()
  "Initialize ctable."
  ;; Register autoload for lib/benchmark
  (autoload 'make-ctbl:cmodel "ctable"))

(use-package ctable
  :defer t
  :config (user--ctable-config))


(provide 'utilities/ctable)
;;; ctable.el ends here
