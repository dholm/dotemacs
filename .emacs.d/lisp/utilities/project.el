;;; project.el --- helpers for working with projects
;;; Commentary:
;;; Code:

(defun user/ede-project (path)
  "Get the EDE project for PATH."
  (let ((project (ede-current-project (expand-file-name path))))
    (when (ede-cpp-root-project-p project)
      project)))


(defun user/ede-project-include-paths (project)
  "List of include paths for EDE PROJECT."
  (when project
    (let ((root-path (ede-project-root-directory project))
          (include-paths (oref project include-path)))
      (mapcar #'(lambda (path) (expand-file-name path root-path)) include-paths))))


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
    (let ((ede-proj (user/ede-project (file-truename path))))
      (cond
       (ede-proj (ede-project-root-directory ede-proj))
       (t (user/ffip-project-root path))))))


(defun user/project-include-paths (path)
  "Get the project include paths for PATH, if it exists."
  (when path
    (let ((ede-proj (user/ede-project (file-truename path))))
      (cond
       (ede-proj (user/ede-project-include-paths ede-proj))
       (t nil)))))


(defun user/project-p (path)
  "Check if PATH is under project control."
  (user/project-root path))


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


(defun user/current-path-apply (fn)
  "Based on the current context apply FN to it."
  (let ((path (or (buffer-file-name) default-directory)))
    (funcall fn path)))


(provide 'utilities/project)
;;; project.el ends here
