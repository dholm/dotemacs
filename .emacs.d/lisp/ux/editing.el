;;; editing.el --- Configure Emacs editing
;;; Commentary:
;;; Code:

(defun user/editing-init ()
  "Initialize editing in Emacs."
  (when (eq window-system 'ns)
    (setq
     ;; Swap command and option on MacOS X.
     mac-option-modifier 'alt
     mac-command-modifier 'meta))

  ;;; (Bindings) ;;;
  (user/bind-key-global :code :fill-paragraph 'fill-paragraph))

(user/editing-init)


(provide 'ux/editing)
;;; editing.el ends here
