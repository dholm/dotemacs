;;; symbols.el --- Configure how Emacs handles certain symbols
;;; Commentary:
;;; Code:

(defun user/page-break-lines-init ()
  "Initialize page break lines."
  (global-page-break-lines-mode t)
  (after-load 'diminish
    (diminish 'page-break-lines-mode)))


(defun user/symbols-init ()
  "Initialize Emacs symbol handling."
  ;;; (Packages) ;;;
  (use-package page-break-lines
    :ensure t
    :config (user/page-break-lines-init))

  (with-feature 'ux/coding
    (when (eq default-terminal-coding-system 'utf-8)
      (use-package pretty-mode-plus
        :ensure t))))

(user/symbols-init)


(provide 'ux/symbols)
;;; symbols.el ends here
