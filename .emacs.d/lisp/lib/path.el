;;; path.el --- support functions for working with paths
;;; Commentary:
;;; Code:

(defun path-abs-buffer ()
  "Get the current buffer absolute path."
  (file-truename (or (buffer-file-name) default-directory)))


(defun path-join (root &rest dirs)
  "Join paths together starting at ROOT and proceeding with DIRS.
Ex: (path-join \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"
  (if (not dirs)
      root
    (apply 'path-join
           (expand-file-name (car dirs) root)
           (cdr dirs))))


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


(provide 'lib/path)
;;; path.el ends here
