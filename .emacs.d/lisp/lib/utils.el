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


(defmacro with-function (function &rest body)
  "If FUNCTION is available, evaluate BODY."
  (declare (indent defun))
  `(when (functionp ,function)
     ,@body))


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


(provide 'lib/utils)
;;; utils.el ends here
