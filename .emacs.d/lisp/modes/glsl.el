;;; glsl.el --- Initializes GLSL mode
;;; Commentary:
;;; Code:

(defun user/glsl-mode-hook ()
  "GLSL mode hook.")


(defun user/glsl-mode-init ()
  "Initialize GLSL mode."
  (add-hook 'glsl-mode-hook 'user/glsl-mode-hook))

(use-package glsl-mode
  :ensure t
  :config (user/glsl-mode-init))


(provide 'modes/glsl)
;;; glsl.el ends here
