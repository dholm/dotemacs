;;; swig.el --- SWIG mode support
;;; Commentary:
;;; Code:

(defun user--swig-mode-hook ()
  "SWIG mode hook.")


(defun user--swig-mode-config ()
  "Initialize SWIG mode."
  (add-hook 'swig-mode-hook 'user--swig-mode-hook)
  (add-auto-mode 'swig-mode "\\.swg$"))


(require-package '(:name swig-mode :after (user--swig-mode-config)))


(provide 'modes/swig)
;;; swig.el ends here
