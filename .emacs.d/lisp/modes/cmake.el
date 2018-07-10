;;; cmake.el --- Initializes CMake mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--cmake-mode-hook ()
  "Initialize makefile mode."
  (unless (derived-mode-p 'prog-mode)
    (user--prog-mode-hook))

  ;; Separate camel-case into separate words.
  (subword-mode t))

(use-package cmake-mode
  :if (executable-find "cmake")
  :defer
  :mode "\(CMakeLists\.txt|\.cmake\)$"
  :hook (cmake-mode-hook . user--cmake-mode-hook)
  :config
  (use-package company-cmake
    :after (company)
    :config
    (add-to-list 'company-backends 'company-cmake)))


(provide 'modes/cmake)
;;; cmake.el ends here
