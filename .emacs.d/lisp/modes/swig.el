;;; swig.el --- SWIG mode support
;;; Commentary:
;;; Code:

(use-package swig-mode
  :defer t
  :quelpa (swig-mode
           :fetcher github
           :repo "dholm/swig-mode")
  :init
  (add-auto-mode 'swig-mode "\\.swg$"))


(provide 'modes/swig)
;;; swig.el ends here
