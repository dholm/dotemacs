;;; systemtap.el --- SystemTap mode support
;;; Commentary:
;;; Code:

(with-executable 'stap
  (use-package systemtap-mode
    :defer t))


(provide 'modes/systemtap)
;;; systemtap.el ends here
