;;; wc-mode.el --- word count in modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package wc-mode
  :defer
  :init
  (user/bind-key-global :util :wc-mode 'wc-mode))


(provide 'utilities/wc-mode)
;;; wc-mode.el ends here
