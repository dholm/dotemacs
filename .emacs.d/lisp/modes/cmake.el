;;; cmake.el --- Initializes CMake mode
;;; Commentary:
;;; Code:

(defun user/cmake-mode-hook ()
  "Initialize makefile mode."
  (unless (derived-mode-p 'prog-mode)
    (user/prog-mode-hook))
  ;; Separate camel-case into separate words.
  (subword-mode t)
  ;; Enable auto-complete.
  (auto-complete-mode t))


(defun user/cmake-mode-init ()
  "Initialize makefile mode."
  (add-hook 'cmake-mode-hook 'user/cmake-mode-hook)

  (add-auto-mode 'cmake-mode "CMakeLists\\.txt$")
  (add-auto-mode 'cmake-mode "\\.cmake$"))

(when *has-cmake*
  (require-package '(:name cmake-mode :after (user/cmake-mode-init))))


(provide 'modes/cmake)
;;; cmake.el ends here