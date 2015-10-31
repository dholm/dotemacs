;;; project.el --- helpers for working with projects
;;; Commentary:
;;; Code:

(after-load 'eieio
  (defclass user/proj ()
    ()
    "Project class.")

  (defgeneric user/proj-name ((project user/proj))
    "Get the PROJECT name.")

  (defgeneric user/proj-root ((project user/proj))
    "Get the PROJECT root path.")

  (defgeneric user/proj-include-paths ((project user/proj))
    "Get the include paths for PROJECT.")

  (defgeneric user/proj-build ((project user/proj))
    "Build PROJECT.")

  (defmethod user/proj-name ((project user/proj))
    (file-name-nondirectory
     (directory-file-name (user/proj-root project))))

  (defmethod user/proj-include-paths ((project user/proj)))

  (after-load 'ede
    (defclass user/ede-proj (user/proj)
      ((ede :initarg :ede :initform nil
            :documentation "EDE project instance."))
      "EDE project class.")

    (defmethod user/proj-from-path :static ((project user/ede-proj) &rest args)
      "Constructor for EDE project at path in ARGS."
      (when (featurep 'ede)
        (let ((ede-proj (ede-current-project (expand-file-name (first args)))))
          (when (cl-some (lambda (args)
                           (and (featurep (first args))
                                (funcall (second args) ede-proj)))
                         '((ede/cpp-root ede-cpp-root-project-p)
                           (ede/generic ede-generic-makefile-project-p)
                           (ede/generic ede-generic-cmake-project-p)
                           (ede/linux ede-linux-project-p)
                           (ede-compdb ede-compdb-project-p)))
            (make-instance 'user/ede-proj :ede ede-proj)))))

    (defmethod user/proj-name ((project user/ede-proj))
      (ede-name (oref project :ede)))

    (defmethod user/proj-root ((project user/ede-proj))
      (ede-project-root-directory (oref project :ede)))

    (defmethod user/proj-include-paths ((project user/ede-proj))
      (let ((root-path (user/proj-root project))
            (include-paths (oref (oref project :ede) include-path)))
        (mapcar #'(lambda (path) (expand-file-name path root-path))
                include-paths)))

    (defmethod user/proj-build ((project user/ede-proj))
      (let ((ede-proj (oref project :ede)))
        (project-compile-project
         ede-proj
         (read-string "Build command: " (oref ede-proj compile-command))))))

  (after-load 'projectile
    (defclass user/projectile-proj (user/proj)
      ((root-path :initarg :root-path
                  :type string
                  :documentation "Project root path."))
      "Projectile project class.")

    (defmethod user/proj-from-path :static ((project user/projectile-proj) &rest args)
      "Constructor for projectile project at path in ARGS."
      (let ((default-directory (file-name-as-directory (first args)))
            (projectile-require-project-root nil))
        (when (projectile-project-p)
          (make-instance 'user/projectile-proj :root-path (projectile-project-root)))))

    (defmethod user/proj-root ((project user/projectile-proj))
      (oref project :root-path)))

  (after-load 'vc
    (defclass user/vc-proj (user/proj)
      ((root-path :initarg :root-path
                  :type string
                  :documentation "Project root path."))
      "VC project class.")

    (defmethod user/proj-from-path :static ((project user/vc-proj) &rest args)
      "Constructor for VC project at path in ARGS."
      (let* ((vc-backend (and (fboundp 'vc-responsible-backend)
                              (ignore-errors
                                (vc-responsible-backend
                                 (file-truename (first args))))))
             (root-path (and vc-backend
                             (vc-call-backend
                              vc-backend 'root (file-truename (first args))))))
        (when root-path
              (make-instance 'user/vc-proj :root-path root-path))))

    (defmethod user/proj-root ((project user/vc-proj))
      (oref project :root-path)))

  (defmethod user/proj-from-path :static ((project user/proj) &rest args)
    "Constructor for project at path in ARGS."
    (let ((f (lambda (proj-type)
               (when (fboundp proj-type)
                 (funcall #'user/proj-from-path proj-type (first args)))))
          (proj-types '(user/ede-proj user/projectile-proj user/vc-proj)))
      (cl-some f proj-types))))


(defmacro with-project (project &optional path &rest body)
  "Bind PROJECT for PATH and evaluate BODY if project exists."
  (declare (indent defun)
           (debug let))
  `(let ((,project (user/proj-from-path user/proj (or ,path (path-abs-buffer)))))
     (when ,project
       ,@body)))


(defmacro with-project-root (project-root &optional path &rest body)
  "Bind PROJECT-ROOT for PATH and evaluate BODY if project exists."
  (declare (indent defun)
           (debug let))
  `(with-project project (or ,path (path-abs-buffer))
     (let ((,project-root (user/proj-root project)))
       ,@body)))


(provide 'utilities/project)
;;; project.el ends here
