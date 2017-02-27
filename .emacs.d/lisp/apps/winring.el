;;; winring.el --- Emacs window manager -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package winring
  :config
  (use-package zoom-window
    :config
    (define-key winring-map (kbd "z") 'zoom-window-zoom)))

(winring-initialize)


(provide 'apps/winring)
;;; winring.el ends here
