;;; popwin.el --- popup window manager for Emacs
;;; Commentary:
;;; Code:

(defun user/popwin-init ()
  "Initialize popwin."
  (popwin-mode t)

  ;; Don't select compilation window when shown
  (push '(compilation-mode :height 20 :dedicated t) popwin:special-display-config)

  ;;; (Bindings) ;;;
  (define-key user/navigation-map (kbd "p") 'popwin:popup-buffer)
  (define-key user/navigation-map (kbd "0") 'popwin:close-popup-window))

(require-package '(:name popwin
                         :prepare (autoload 'popwin-mode "popwin")
                         :load-path ("." "misc")
                         :after (user/popwin-init)))


(provide 'ux/popwin)
;;; popwin.el ends here
