;;; popups.el --- Configure Emacs popups
;;; Commentary:
;;; Code:

(defun user/popup-init ()
  "Initialize popup."
  ;; Install workaround for whitespace-mode bug.
  (after-load 'modes/whitespace
    (defadvice popup-draw (before user/turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing popup."
      (user/whitespace-mode-suppress t))

    (defadvice popup-delete (after user/restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting popup."
      (user/whitespace-mode-suppress nil))))


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
