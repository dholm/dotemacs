;;; popwin.el --- popup window manager for Emacs
;;; Commentary:
;;; Code:

(defun user/popwin-init ()
  "Initialize popwin."
  (popwin-mode t)

  ;; Don't select compilation window when shown
  (push '(compilation-mode :noselect t) popwin:special-display-config)

  ;;; (Bindings) ;;;
  (define-key user/navigation-map (kbd "p") 'popwin:display-last-buffer)
  (define-key user/navigation-map (kbd "m") 'popwin:messages))

(require-package '(:name popwin
                         :prepare (autoload 'popwin-mode "popwin")
                         :after (user/popwin-init)))


(provide 'ux/popwin)
;;; popwin.el ends here
