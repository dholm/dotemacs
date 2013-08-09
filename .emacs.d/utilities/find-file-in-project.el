;;; find-file-in-project.el --- Quickly locate files within current project
;;; Commentary:
;;; Code:

(defun user/find-file-in-project-init ()
  "Initialize find file in project."
  (global-set-key (kbd "C-x f") 'find-file-in-project))

(require-package '(:name find-file-in-project :after (user/find-file-in-project-init)))


(provide 'utilities/find-file-in-project)
;;; find-file-in-project.el ends here
