;;; systemtap.el --- SystemTap mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-executable 'stap
  (use-package systemtap-mode
    :defer))


(provide 'modes/systemtap)
;;; systemtap.el ends here
