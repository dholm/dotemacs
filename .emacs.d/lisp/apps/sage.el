;;; sage.el --- Sage integration with Emacs
;;; Commentary:
;;; Code:

(defun user/sage-startup-hook ()
  "Sage startup hook."
  ;; Support typeset output.
  (sage-view))


(defun user/sage-mode-init ()
  "Initialize Sage mode."
  (add-hook 'sage-startup-after-prompt-hook 'user/sage-startup-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :sage 'sage))

(with-executable 'sage
  (req-package sage-mode
    :loader :el-get
    :config (user/sage-mode-init)))


(provide 'apps/sage)
;;; sage.el ends here
