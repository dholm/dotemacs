;;; wc-mode.el --- word count in modeline
;;; Commentary:
;;; Code:

(use-package wc-mode
  :defer t
  :init
  (user/bind-key-global :util :wc-mode 'wc-mode))


(provide 'utilities/wc-mode)
;;; wc-mode.el ends here
