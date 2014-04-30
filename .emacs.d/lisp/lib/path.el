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


(provide 'lib/path)
;;; path.el ends here
