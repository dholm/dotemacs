;;; utils.el --- miscellaneous support functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defmacro try-eval (fn &optional finally)
  "Safely evaluate expression FN and run FINALLY after."
  (declare (debug t)
           (indent 1))
  `(let (retval)
     (condition-case-unless-debug ex
         (setq retval (progn ,fn))
       ('error
        (setq retval (cons 'exception (list ex)))))
     ,@finally
     retval))


(defun feature-p (feature)
  "Check if FEATURE is available."
  (or (featurep feature)
      (when (functionp 'package-installed-p)
        (package-installed-p feature))
      (locate-library (symbol-name feature))))


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
