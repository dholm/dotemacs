;;; with.el --- conditional eval wrappers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defmacro with-feature (feature &rest body)
  "If FEATURE is available, load it and evaluate BODY."
  (declare (indent defun))
  `(when (require ,feature nil :noerror)
     ,@body))


(defmacro with-function (function &rest body)
  "If FUNCTION is available, evaluate BODY."
  (declare (indent defun))
  `(when (functionp ,function)
     ,@body))


(defmacro with-executable (executable &rest body)
  "If EXECUTABLE is available in path, evaluate BODY."
  (declare (indent defun))
  `(when (executable-find (symbol-name ,executable))
     ,@body))


(defmacro with-any-executable (executables &rest body)
  "If any of EXECUTABLES are available in the path, evaluate BODY."
  (declare (indent defun))
  `(when (some (lambda (x) (executable-find (symbol-name x))) ,executables)
     ,@body))


(provide 'lib/with)
;;; with.el ends here
