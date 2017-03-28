;;; wttrin.el --- Emacs weather service. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package wttrin
  :defer
  :init
  (user/bind-key-global :apps :weather 'wttrin))


(provide 'apps/wttrin)
;;; wttrin.el ends here
