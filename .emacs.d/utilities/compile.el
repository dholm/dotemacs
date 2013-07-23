;;; compile --- sets up Emacs compile support
;;; Commentary:
;;; Code:

(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)


(defun dholm/gen-std-compile-string ()
  "Generates compilation string for standard GNU Make project"
  (let* ((current-dir (file-name-directory
      		 (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj)))
    (concat "cd " root-dir "; nice make -j")))


(provide 'utilities/compile)
;;; compile.el ends here
