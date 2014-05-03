;;; string.el --- Emacs string functions.
;;; Commentary:
;;; Code:

(defmacro with-face (str &rest properties)
  "Print STR using PROPERTIES."
  `(propertize ,str 'face (list ,@properties)))


(provide 'lib/string)
;;; string.el ends here
