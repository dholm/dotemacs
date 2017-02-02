;;; view.el --- Emacs view mode setup
;;; Commentary:
;;; Code:

(defun user--view-mode-config ()
  "Initialize view mode."
  (validate-setq
   ;; Open read-only files in view-mode by default.
   view-read-only t))

(user--view-mode-config)


(provide 'modes/view)
;;; view.el ends here
