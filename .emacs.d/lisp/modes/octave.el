;;; octave.el --- Octave mode
;;; Commentary:
;;; Code:

(defun user/octave-mode-hook ()
  "Initialize Octave mode."
  (when (el-get-package-is-installed 'ac-octave)
    (add-ac-sources 'ac-octave)))


(defun user/octave-init ()
  "Initialize Emacs Octave support."
  (add-auto-mode 'octave-mode "\\.m$")
  (add-hook 'octave-mode-hook 'user/octave-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name ac-octave)))

(with-executable 'octave
  (user/octave-init))


(provide 'modes/octave)
;;; octave.el ends here
