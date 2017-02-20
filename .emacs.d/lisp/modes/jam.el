;;; jam.el --- Initializes Jam mode
;;; Commentary:
;;; Code:

(defun user--jam-mode-hook ()
  "Initialize Jam mode."
  ;; Use spaces for indent
  (setq indent-tabs-mode nil))

(use-package jam-mode
  :defer t
  :init
  (add-hook 'jam-mode-hook 'user--jam-mode-hook)
  (add-auto-mode 'jam-mode "\\.jam$" "Jamfile.*")
  :config
  (validate-setq
   ;; Default indent width.
   jam-indent-size 4))


(provide 'modes/jam)
;;; jam.el ends here
