;;; symbols.el --- Configure how Emacs handles certain symbols
;;; Commentary:
;;; Code:

(defun user--page-break-lines-config ()
  "Initialize page break lines."
  (global-page-break-lines-mode t)
  (after-load 'diminish
    (diminish 'page-break-lines-mode)))


(defun user--symbols-config ()
  "Initialize Emacs symbol handling."
  ;;; (Packages) ;;;
  (req-package page-break-lines
    :config (user--page-break-lines-config))

  (with-feature 'ux/coding
    (when (eq default-terminal-coding-system 'utf-8)
      (req-package pretty-mode-plus))))

(user--symbols-config)


(provide 'ux/symbols)
;;; symbols.el ends here
