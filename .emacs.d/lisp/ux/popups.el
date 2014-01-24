;;; popups.el --- Configure Emacs popups
;;; Commentary:
;;; Code:

(defun user/popups-init ()
  "Initialize Emacs popups."
  (require-package '(:name popup :after (user/popup-init)))
  (require-package '(:name popwin
                           :prepare (autoload 'popwin-mode "popwin")
                           :load-path ("." "misc")
                           :after (user/popwin-init))))


(defun user/popup-init ()
  "Initialize popup.")


(defun user/popwin-init ()
  "Initialize popwin."
  (popwin-mode t)

  ;; Don't select compilation window when shown
  (push '(compilation-mode :height 20 :dedicated t) popwin:special-display-config)

  ;;; (Bindings) ;;;
  (define-key user/view-map (kbd "p") 'popwin:popup-buffer)
  (define-key user/view-map (kbd "0") 'popwin:close-popup-window))


(user/popups-init)


(provide 'ux/popups)
;;; popups.el ends here
