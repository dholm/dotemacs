;;; symbols.el --- Configure how Emacs handles certain symbols -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package page-break-lines
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode t))

(with-eval-after-load 'ux/coding
  (when (eq default-terminal-coding-system 'utf-8)
    (use-package pretty-mode-plus)))

(use-package all-the-icons
  :if window-system)


(provide 'ux/symbols)
;;; symbols.el ends here
