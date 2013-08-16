;;; project.el --- helpers for working with projects
;;; Commentary:
;;; Code:

(defun user/ede-project (path)
  "Get the EDE project for PATH."
  (let ((project (ede-current-project (expand-file-name path))))
    (when (ede-cpp-root-project-p project)
      project)))


(defun user/ffip-project-root (path)
  "Use find-file-in-project to locate root from PATH."
  (let ((path-directory (file-name-directory path)))
    (or ffip-project-root
       (if (functionp ffip-project-root-function)
           (funcall ffip-project-root-function)
         (if (listp ffip-project-file)
             (some (apply-partially 'locate-dominating-file
                                    path-directory)
                   ffip-project-file)
           (locate-dominating-file path-directory
                                   ffip-project-file))))))


(defun user/project-root (path)
  "Get the project root for PATH, if it exists."
  (let ((ede-proj (user/ede-project path)))
    (cond
     (ede-proj (ede-project-root ede-proj))
     ((featurep 'find-file-in-project) (user/ffip-project-root path)))))


(provide 'utilities/project)
;;; project.el ends here
