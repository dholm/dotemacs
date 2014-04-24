;;; popups.el --- Configure Emacs popups
;;; Commentary:
;;; Code:

(defun user/popup-init ()
  "Initialize popup.")


(defun user/popwin-init ()
  "Initialize popwin."
  (with-feature 'popwin
    (popwin-mode t)

    ;; Don't select compilation window when shown
    (push '(compilation-mode :height 20 :dedicated t)
          popwin:special-display-config)

    ;;; (Bindings) ;;;
    (user/bind-key-global :util :popwin-close 'popwin:close-popup-window)
    (user/bind-key-global :util :popwin-buffer 'popwin:popup-buffer)))


(defun user/popups-init ()
  "Initialize Emacs popups."
  (setq-default
   ;; Timeout for messages shown in minibuffer.
   minibuffer-message-timeout 5)

  ;;; (Packages) ;;;
  (require-package '(:name popup :after (user/popup-init)))
  (require-package '(:name popwin :after (user/popwin-init))))

(user/popups-init)


(provide 'ux/popups)
;;; popups.el ends here
