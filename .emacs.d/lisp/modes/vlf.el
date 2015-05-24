;;; vlf --- Initializes view large file mode
;;; Commentary:
;;; Code:

(defun user/vlf-mode-hook ()
  "View large file mode hook.")


(defun user/vlfi-init ()
  "Initialize view large file mode."
  (add-hook 'vlf-mode-hook 'user/vlf-mode-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :view-file 'vlf))

(require-package '(:name vlfi :after (user/vlfi-init)))


(provide 'modes/vlf)
;;; vlf.el ends here
