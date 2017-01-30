;;; wc-mode.el --- word count in modeline
;;; Commentary:
;;; Code:

(defun user--wc-mode-config ()
  "Initialize wc-mode."
  (user/bind-key-global :util :wc-mode 'wc-mode))

(use-package wc-mode
  :defer t
  :config (user--wc-mode-config))


(provide 'utilities/wc-mode)
;;; wc-mode.el ends here
