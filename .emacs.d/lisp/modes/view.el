;;; view.el --- Emacs view mode setup
;;; Commentary:
;;; Code:

(defun user/view-mode-init ()
  "Initialize view mode."
  (setq-default
   ;; Open read-only files in view-mode by default.
   view-read-only t))

(user/view-mode-init)


(provide 'modes/view)
;;; view.el ends here
