;;; symbols.el --- Configure how Emacs handles certain symbols
;;; Commentary:
;;; Code:

(defun user/page-break-lines-init ()
  "Initialize page break lines."
  (require 'page-break-lines)
  (global-page-break-lines-mode)
  (after-load 'diminish
    (diminish 'page-break-lines-mode)))


(defun user/pretty-mode-init ()
  "Initialize pretty mode."
  ;; Enable pretty mode plus globally
  (global-pretty-mode t))


(defun user/symbols-init ()
  "Initialize Emacs symbol handling."
  ;;; (Packages) ;;;
  (require-package '(:name page-break-lines :after (user/page-break-lines-init)))

  (require 'ux/coding)
  (when (eq default-terminal-coding-system 'utf-8)
    (require-package '(:name pretty-mode-plus :after (user/pretty-mode-init)))))

(user/symbols-init)


(provide 'ux/symbols)
;;; symbols.el ends here
