;;; symbols.el --- Configure how Emacs handles certain symbols -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package page-break-lines
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode t))

(use-package all-the-icons
  :if window-system)


(provide 'ux/symbols)
;;; symbols.el ends here
