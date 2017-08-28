;;; swig.el --- SWIG mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package swig-mode
  :defer
  :quelpa (swig-mode
           :fetcher github
           :repo "dholm/swig-mode")
  :mode "\\.swg$")


(provide 'modes/swig)
;;; swig.el ends here
