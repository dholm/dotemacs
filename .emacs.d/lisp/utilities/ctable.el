;;; ctable --- Table component for Emacs
;;; Commentary:
;;; Code:

(defun user/ctable-init ()
  "Initialize ctable."
  ;; Register autoload for lib/benchmark
  (autoload 'make-ctbl:cmodel "ctable"))

(use-package ctable
  :ensure t
  :config (user/ctable-init))


(provide 'utilities/ctable)
;;; ctable.el ends here
