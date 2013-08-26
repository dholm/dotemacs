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
  (when (el-get-package-is-installed 'find-file-in-project)
    (require 'find-file-in-project)
    (let ((path-directory (file-name-directory path)))
      (or ffip-project-root
         (if (functionp ffip-project-root-function)
             (funcall ffip-project-root-function)
           (if (listp ffip-project-file)
               (some (apply-partially 'locate-dominating-file
                                      path-directory)
                     ffip-project-file)
             (locate-dominating-file path-directory
                                     ffip-project-file)))))))


(defun user/project-root (path)
  "Get the project root for PATH, if it exists."
  (when path
    (let* ((path (file-truename path))
           (ede-proj (user/ede-project path)))
      (cond
       (ede-proj (ede-project-root-directory ede-proj))
       (t (user/ffip-project-root path))))))


(defun user/project-root-p (path)
  "Check if PATH represents a project root."
  (let ((proj-root (user/project-root path)))
    (and path proj-root (equal (file-truename path) (file-truename proj-root)))))


(defun user/gnu-global-tags-location (path)
  "Get the location of Global's database from PATH, if it exists."
  (let ((gtags-file-name "GTAGS")
        (proj-root (user/project-root path)))
    (when (file-exists-p (path-join proj-root gtags-file-name))
      proj-root)))


(defun user/gnu-global-tags-p (path)
  "Check if a Global tag database exists for project in PATH."
  (when (user/gnu-global-tags-location path)
    t))


(provide 'utilities/project)
;;; project.el ends here
