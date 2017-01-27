;;; swig.el --- SWIG mode support
;;; Commentary:
;;; Code:

(defun user/swig-mode-hook ()
  "SWIG mode hook.")


(defun user/swig-mode-init ()
  "Initialize SWIG mode."
  (add-hook 'swig-mode-hook 'user/swig-mode-hook)
  (add-auto-mode 'swig-mode "\\.swg$"))


(req-package swig-mode
  :loader :el-get
  :config (user/swig-mode-init))


(provide 'modes/swig)
;;; swig.el ends here
