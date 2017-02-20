;;; swig.el --- SWIG mode support
;;; Commentary:
;;; Code:

(defun user--swig-mode-hook ()
  "SWIG mode hook.")

(use-package swig-mode
  :defer t
  :quelpa (swig-mode
           :fetcher github
           :repo "dholm/swig-mode")
  :config
  (add-hook 'swig-mode-hook 'user--swig-mode-hook)
  (add-auto-mode 'swig-mode "\\.swg$"))


(provide 'modes/swig)
;;; swig.el ends here
