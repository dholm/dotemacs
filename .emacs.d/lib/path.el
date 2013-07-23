;;; path --- support functions for working with paths
;;; Commentary:
;;; Code:

(defun path-join (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
   (path-join \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"

  (if (not dirs)
      root
    (apply 'path-join
           (expand-file-name (car dirs) root)
           (cdr dirs))))


(provide 'lib/path)
;;; path.el ends here
