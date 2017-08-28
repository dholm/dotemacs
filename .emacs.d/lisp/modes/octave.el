;;; octave.el --- Octave mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--octave-mode-hook ()
  "Initialize Octave mode."
  (when (feature-p 'ac-octave)
    (add-ac-sources 'ac-octave))

  (eldoc-mode t))

(use-package octave
  :ensure nil
  :defer
  :mode ("\.m$" . octave-mode)
  :init
  (add-hook 'octave-mode-hook 'user--octave-mode-hook)
  :config
  (use-package ac-octave
    :disabled
    :after auto-complete
    :defer))


(provide 'modes/octave)
;;; octave.el ends here
