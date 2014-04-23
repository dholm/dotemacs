;;; utils.el --- miscellaneous support functions
;;; Commentary:
;;; Code:

(defmacro try-eval (fn &optional finally)
  "Safely evaluate expression FN and run FINALLY after."
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@finally))


(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


(defun feature-p (feature)
  "Check if FEATURE is available."
  (or (featurep feature)
      (el-get-package-is-installed feature)
      (locate-library (symbol-name feature))))


(defmacro with-feature (feature &rest body)
  "If FEATURE is available, load it and evaluate BODY."
  (declare (indent defun))
  `(when (require ,feature nil :noerror)
     ,@body))


(defmacro with-executable (executable &rest body)
  "If EXECUTABLE is available in path, evaluate BODY."
  (declare (indent defun))
  `(when (executable-find (symbol-name ,executable))
     ,@body))


(defun add-many-to-list (the-list &rest entries)
  "Add to THE-LIST any specified ENTRIES."
  (dolist (entry entries)
    (add-to-list the-list entry))
  (eval the-list))

(defmacro add-many-to-list-after-load (the-list feature &rest entries)
  "Add to THE-LIST after loading FEATURE any specified ENTRIES."
  `(after-load ,feature
     (add-many-to-list ,the-list ,@entries)))


(defun add-command-switch (handler &rest switch-list)
  "Add HANDLER for SWITCH-LIST."
  (dolist (switch switch-list)
    (add-to-list 'command-switch-alist (cons switch handler))))


(defun add-auto-mode (mode &rest patterns)
  "Use `MODE' for all given files matching `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


(defun add-magic-mode (mode &rest patterns)
  "Use `MODE' for all files containing header `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'magic-mode-alist (cons pattern mode))))


(defun add-interpreter-mode (mode &rest interpreters)
  "Use `MODE' for all files with shebang `INTERPRETERS'."
  (dolist (interpreter interpreters)
    (add-to-list 'interpreter-mode-alist (cons interpreter mode))))


(defun user/all-asscs (asslist query)
  "A list of all values in ASSLIST corresponding to QUERY (like rassoc)."
  (cond
   ((null asslist) nil)
   (t
    (if (equal (cdr (car asslist)) query)
        (cons (car (car asslist))
              (user/all-asscs (cdr asslist) query))
      (user/all-asscs (cdr asslist) query)))))


(provide 'lib/utils)
;;; utils.el ends here
