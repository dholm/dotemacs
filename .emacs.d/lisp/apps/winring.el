;;; winring.el --- Emacs window manager
;;; Commentary:
;;; Code:

(defun user/winring-init ()
  "Initialize winring."
  (winring-initialize)

  ;;; (Bindings) ;;;
  (when (feature-p 'zoom-window)
    (define-key winring-map (kbd "z") 'zoom-window-zoom)))

(req-package winring
  :config (user/winring-init))
(req-package zoom-window)


(provide 'apps/winring)
;;; winring.el ends here
