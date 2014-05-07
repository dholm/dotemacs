;;; man.el --- Man page support
;;; Commentary:
;;; Code:

(defun user/man-mode-init ()
  "Initialize Emacs man-mode."
  (setq-default
   ;; Make man-mode wrap lines at half the width of Emacs.
   Man-width (/ (window-total-width (frame-root-window)) 2))

  ;;; (Bindings) ;;;
  (user/bind-key-global :doc :manual 'man))

(with-executable 'man
  (user/man-mode-init))


(provide 'modes/man)
;;; man.el ends here
