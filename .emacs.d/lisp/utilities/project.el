;;; project.el --- helpers for working with projects
;;; Commentary:
;;; Code:

(defun user/ede-project (path)
  "Get the EDE project for PATH."
  (with-feature 'ede/cpp-root
    (let ((project (ede-current-project (expand-file-name path))))
      (when (and (featurep 'ede/cpp-root)
                 (ede-cpp-root-project-p project))
        project))))


(defun user/ede-project-include-paths (project)
  "List of include paths for EDE PROJECT."
  (when project
    (let ((root-path (ede-project-root-directory project))
          (include-paths (oref project include-path)))
      (mapcar #'(lambda (path) (expand-file-name path root-path))
              include-paths))))


(defun user/projectile-project-root (path)
  "Use projectile to locate root from PATH."
  (with-feature 'projectile
    (file-truename
     (let ((dir (file-truename path)))
       (or (--reduce-from
            (or acc (funcall it dir)) nil
            projectile-project-root-files-functions)
           path)))))


(defun user/project-root (path)
  "Get the project root for PATH, if it exists."
  (when path
    (let ((ede-proj (user/ede-project (file-truename path)))
          (vc-backend (and (fboundp 'vc-responsible-backend)
                           (ignore-errors
                             (vc-responsible-backend (file-truename path))))))
      (cond
       (ede-proj (ede-project-root-directory ede-proj))
       (vc-backend (vc-call-backend vc-backend 'root (file-truename path)))
       (t (user/projectile-project-root path))))))


(defun user/project-include-paths (path)
  "Get the project include paths for PATH, if it exists."
  (when path
    (let ((ede-proj (user/ede-project (file-truename path))))
      (cond
       (ede-proj (user/ede-project-include-paths ede-proj))
       (t nil)))))


(defun user/project-name (path)
  "Name of project at PATH."
  (when path
    (let ((ede-proj (user/ede-project (file-truename path))))
      (cond
       (ede-proj (ede-name ede-proj))
       (t (file-name-nondirectory
           (directory-file-name (user/projectile-project-root path))))))))


(defun user/project-p (path)
  "Check if PATH is under project control."
  (user/project-root path))


(defmacro with-project-root (project-root &optional path &rest body)
  "Bind PROJECT-ROOT for PATH and evaluate BODY if project exists."
  (declare (indent defun)
           (debug let))
  `(let ((,project-root (user/project-root (or ,path (path-abs-buffer)))))
     (when ,project-root
       ,@body)))


(defmacro with-ede-project (ede-project &optional path &rest body)
  "Bind EDE-PROJECT for PATH and evaluate BODY if project exists."
  (declare (indent defun)
           (debug with-project-root))
  `(let ((,ede-project (user/ede-project
                        (user/project-root (or ,path (path-abs-buffer))))))
     (when ,ede-project
       ,@body)))


(defun user/project-root-p (path)
  "Check if PATH represents a project root."
  (with-project-root proj-root path
    (and path proj-root (equal (file-truename path) (file-truename proj-root)))))


(defun user/gnu-global-tags-location (path)
  "Get the location of Global's database from PATH, if it exists."
  (with-project-root proj-root path
    (when (file-exists-p (path-join proj-root "GTAGS"))
      proj-root)))


(defun user/gnu-global-tags-p (path)
  "Check if a GNU Global tag database exists for project in PATH."
  (when (user/gnu-global-tags-location path)
    t))


(provide 'utilities/project)
;;; project.el ends here
