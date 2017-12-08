;;; sauron.el --- Emacs event tracker -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package sauron
  :defer
  :init
  (user/bind-key-global :util :notifications 'sauron-toggle-hide-show)

  (sauron-start-hidden)
  :config
  (validate-setq
   ;; Display sauron in current frame.
   sauron-separate-frame nil)

  (with-eval-after-load 'alert
    (add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)))


(provide 'utilities/sauron)
;;; sauron.el ends here
