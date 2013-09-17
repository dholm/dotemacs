;;; swig.el --- SWIG mode support
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl))


(defun user/swig-mode-hook ()
  "SWIG mode hook.")


(defun user/swig-mode-init ()
  "Initialize SWIG mode."
  (add-hook 'swig-mode-hook 'user/swig-mode-hook)
  (add-auto-mode 'swig-mode "\\.swg$"))


(require-package '(:name swig-mode
                         :type github
                         :pkgname "dholm/swig-mode"
                         :after (user/swig-mode-init)))


(provide 'modes/swig)
;;; swig.el ends here
