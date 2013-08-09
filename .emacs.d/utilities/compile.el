;;; compile.el --- sets up Emacs compile support
;;; Commentary:
;;; Code:

(require 'compile)

(defun user/compile-init ()
  "Initialize compile module."
  (setq-default
   compilation-disable-input nil
   compilation-scroll-output t
   mode-compile-always-save-buffer-p t)

  ;;; (Bindings) ;;;
  (define-key user/code-map (kbd "c") 'compile)

  ;;; (Functions) ;;;
  (defun user/gen-std-compile-string ()
    "Generate compilation string for standard GNU Make project."
    (let* ((current-dir (file-name-directory
                         (or (buffer-file-name (current-buffer)) default-directory)))
           (prj (ede-current-project current-dir))
           (root-dir (ede-project-root-directory prj)))
      (concat "cd " root-dir "; nice make -j"))))

(user/compile-init)


(provide 'utilities/compile)
;;; compile.el ends here
