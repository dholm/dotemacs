;;; cmake.el --- Initializes CMake mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--cmake-mode-hook ()
  "Initialize makefile mode."
  (unless (derived-mode-p 'prog-mode)
    (user--prog-mode-hook))

  (validate-setq
   ;; Indent using spaces.
   indent-tabs-mode nil)

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
    (add-to-list 'company-backends 'company-cmake))

  (use-package cmake-font-lock
    :hook (cmake-mode-hook . cmake-font-lock-activate)))


(provide 'modes/cmake)
;;; cmake.el ends here
