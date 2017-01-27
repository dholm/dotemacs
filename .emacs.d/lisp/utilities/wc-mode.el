;;; wc-mode.el --- word count in modeline
;;; Commentary:
;;; Code:

(defun user/wc-mode-init ()
  "Initialize wc-mode."
  (user/bind-key-global :util :wc-mode 'wc-mode))

(req-package wc-mode
  :config (user/wc-mode-init))


(provide 'utilities/wc-mode)
;;; wc-mode.el ends here
