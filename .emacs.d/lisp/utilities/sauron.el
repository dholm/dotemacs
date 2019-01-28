;;; sauron.el --- Emacs event tracker -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package sauron
  :defer
  :bind-wrap
  ((:key :util :notifications) . sauron-toggle-hide-show)
  :config
  (validate-setq
   ;; Display sauron in current frame.
   sauron-separate-frame nil)

  (with-eval-after-load 'alert
    (add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter))

  (sauron-start-hidden))


(provide 'utilities/sauron)
;;; sauron.el ends here
