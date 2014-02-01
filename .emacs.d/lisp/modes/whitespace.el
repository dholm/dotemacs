;;; whitespace.el --- whitespace mode support
;;; Commentary:
;;; Code:

(defun user/whitespace-init ()
  "Initialize whitespace mode."
  (setq-default
   ;; Always show trailing whitespace
   show-trailing-whitespace t
   whitespace-style '(trailing lines space-before-tab indentation space-after-tab)
   whitespace-line-column 400)

  ;; Enable whitespace mode globally
  (global-whitespace-mode t)
  (after-load 'diminish
    (diminish 'global-whitespace-mode)))

(user/whitespace-init)


(provide 'modes/whitespace)
;;; whitespace.el ends here
