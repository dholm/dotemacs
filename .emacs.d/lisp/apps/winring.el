;;; winring.el --- Emacs window manager
;;; Commentary:
;;; Code:

(defun user--winring-config ()
  "Initialize winring."
  (winring-initialize)

  ;;; (Bindings) ;;;
  (when (feature-p 'zoom-window)
    (define-key winring-map (kbd "z") 'zoom-window-zoom)))

(use-package winring
  :defer t
  :config (user--winring-config))
(use-package zoom-window
  :defer t)


(provide 'apps/winring)
;;; winring.el ends here
