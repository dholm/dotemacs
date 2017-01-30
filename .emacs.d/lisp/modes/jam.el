;;; jam.el --- Initializes Jam mode
;;; Commentary:
;;; Code:

(defun user--jam-mode-hook ()
  "Initialize Jam mode."
  ;; Use spaces for indent
  (setq indent-tabs-mode nil))


(defun user--jam-mode-config ()
  "Initialize Jam mode."
  (setq-default
   ;; Default indent width.
   jam-indent-size 4)

  (add-hook 'jam-mode-hook 'user--jam-mode-hook)

  (add-auto-mode 'jam-mode "\\.jam$" "Jamfile.*"))

(use-package jam-mode
  :ensure t
  :config (user--jam-mode-config))


(provide 'modes/jam)
;;; jam.el ends here
