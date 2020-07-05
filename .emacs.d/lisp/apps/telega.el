;;; telega.el --- Telegram client -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package telega
  :pin "MELPA"
  :commands (telega)
  :config
  (when (or (eq system-type 'gnu/linux)
            (eq system-type 'darwin))
    (telega-notifications-mode t)))


(provide 'apps/telega)
;;; telega.el ends here
