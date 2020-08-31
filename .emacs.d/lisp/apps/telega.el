;;; telega.el --- Telegram client -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package telega
  :pin "MELPA"
  :commands (telega)
  :hook ((telega-load-hook . telega-mode-line-mode)
         (telega-load-hook . telega-notifications-mode)
         (telega-load-hook . global-telega-squash-message-mode))
  :config
  (when (feature-p 'alert)
    (telega-alert-mode 1)))


(provide 'apps/telega)
;;; telega.el ends here
