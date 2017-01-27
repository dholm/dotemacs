;;; octave.el --- Octave mode
;;; Commentary:
;;; Code:

(defun user/octave-mode-hook ()
  "Initialize Octave mode."
  (when (feature-p 'ac-octave)
    (add-ac-sources 'ac-octave))

  (eldoc-mode t))


(defun user/octave-init ()
  "Initialize Emacs Octave support."
  (add-auto-mode 'octave-mode "\\.m$")
  (add-hook 'octave-mode-hook 'user/octave-mode-hook)

  ;;; (Packages) ;;;
  (use-package ac-octave
    :defer t))

(with-executable 'octave
  (user/octave-init))


(provide 'modes/octave)
;;; octave.el ends here
