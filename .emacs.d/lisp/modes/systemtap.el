;;; systemtap.el --- SystemTap mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package systemtap-mode
  :if (executable-find "stap")
  :defer)


(provide 'modes/systemtap)
;;; systemtap.el ends here
