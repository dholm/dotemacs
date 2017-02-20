;;; octave.el --- Octave mode
;;; Commentary:
;;; Code:

(defun user--octave-mode-hook ()
  "Initialize Octave mode."
  (when (feature-p 'ac-octave)
    (add-ac-sources 'ac-octave))

  (eldoc-mode t))

(use-package octave
  :defer t
  :init
  (add-auto-mode 'octave-mode "\\.m$")
  (add-hook 'octave-mode-hook 'user--octave-mode-hook)
  :config
  (use-package ac-octave))


(provide 'modes/octave)
;;; octave.el ends here
