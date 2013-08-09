;;; find-file-in-project.el --- Quickly locate files within current project
;;; Commentary:
;;; Code:

(defun user/find-file-in-project-init ()
  "Initialize find file in project."
  (setq-default
   ;; Limit search to 4096 hits
   ffip-limit 4096)

  ;;; (Bindings) ;;;
  (global-set-key (kbd "C-x f") 'find-file-in-project)

  ;;; (Functions) ;;;
  ;; Stolen from https://github.com/magnars/.emacs.d/blob/master/setup-ffip.el
  (defun ffip--create-exclude-find-options (names)
    (mapconcat (lambda (name)
                 (concat "-not -regex \".*" name ".*\"")) names " "))

  (defun user/ffip-local-excludes (&rest names)
    "Given a set of NAMES, will exclude results with those names in the path.

Example: (ffip-local-excludes \"target\" \"overlays\")"
    (set (make-local-variable 'ffip-find-options)
         (ffip--create-exclude-find-options names)))

  (defun user/ffip-local-patterns (&rest patterns)
    "An exhaustive list of file name PATTERNS to look for.

Example: (ffip-local-patterns \"*.js\" \"*.jsp\" \"*.css\")"
    (set (make-local-variable 'ffip-patterns) patterns))

  ;; Function to create new functions that look for a specific pattern
  (defun ffip-create-pattern-file-finder (&rest patterns)
    (lexical-let ((patterns patterns))
      (lambda ()
        (interactive)
        (let ((ffip-patterns patterns))
          (find-file-in-project))))))


(require-package '(:name find-file-in-project :after (user/find-file-in-project-init)))


(provide 'utilities/find-file-in-project)
;;; find-file-in-project.el ends here
