;;; jam.el --- Initializes Jam mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--jam-mode-hook ()
  "Initialize Jam mode."
  (validate-setq
   ;; Use spaces for indentation.
   indent-tabs-mode nil))

(use-package jam-mode
  :defer
  :mode "\(\.jam\|Jamfile.*\|Jamroot\)$"
  :hook (jam-mode-hook . user--jam-mode-hook)
  :config
  (validate-setq
   ;; Default indent width.
   jam-indent-size 4))


(provide 'modes/jam)
;;; jam.el ends here
