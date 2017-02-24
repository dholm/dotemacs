;;; symbols.el --- Configure how Emacs handles certain symbols
;;; Commentary:
;;; Code:

(defun user--symbols-config ()
  "Initialize Emacs symbol handling."
  ;;; (Packages) ;;;
  (use-package page-break-lines
    :diminish page-break-lines-mode
    :init
    (global-page-break-lines-mode t))

  (with-feature 'ux/coding
    (when (eq default-terminal-coding-system 'utf-8)
      (use-package pretty-mode-plus))))

(user--symbols-config)


(provide 'ux/symbols)
;;; symbols.el ends here
