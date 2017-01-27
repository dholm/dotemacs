;;; octave.el --- Octave mode
;;; Commentary:
;;; Code:

(defun user--octave-mode-hook ()
  "Initialize Octave mode."
  (when (el-get-package-is-installed 'ac-octave)
    (add-ac-sources 'ac-octave))

  (eldoc-mode t))


(defun user--octave-config ()
  "Initialize Emacs Octave support."
  (add-auto-mode 'octave-mode "\\.m$")
  (add-hook 'octave-mode-hook 'user--octave-mode-hook)

  ;;; (Packages) ;;;
  (req-package ac-octave))

(with-executable 'octave
  (user--octave-config))


(provide 'modes/octave)
;;; octave.el ends here
