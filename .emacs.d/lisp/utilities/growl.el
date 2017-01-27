;;; growl.el --- Growl notification support
;;; Commentary:
;;; Code:

(defun user--growl-config ()
  "Initialize Growl support for Emacs."
  (req-package growl
    :loader :el-get))

(when (osx-app-installed-p "com.growl.growlhelperapp")
  (user--growl-config))


(provide 'utilities/growl)
;;; growl.el ends here
