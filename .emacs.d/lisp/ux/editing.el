;;; editing.el --- Configure Emacs editing
;;; Commentary:
;;; Code:

(defun user/editing-init ()
  "Initialize editing in Emacs."
  ;;; (Bindings) ;;;
  (user/bind-key-global :code :fill-paragraph 'fill-paragraph))

(user/editing-init)


(provide 'ux/editing)
;;; editing.el ends here
