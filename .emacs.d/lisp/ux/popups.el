;;; popups.el --- Configure Emacs popups
;;; Commentary:
;;; Code:

(defun user--popups-config ()
  "Initialize Emacs popups."
  (validate-setq
   ;; Timeout for messages shown in minibuffer.
   minibuffer-message-timeout 5)

  ;;; (Packages) ;;;
  (use-package popup
    :config
    ;; Install workaround for whitespace-mode bug.
    (after-load 'modes/whitespace
      (defadvice popup-draw (before user/turn-off-whitespace activate compile)
        "Turn off whitespace mode before showing popup."
        (user/whitespace-mode-suppress t))

      (defadvice popup-delete (after user/restore-whitespace activate compile)
        "Restore previous whitespace mode when deleting popup."
        (user/whitespace-mode-suppress nil))))

  (use-package popwin
    :init
    (user/bind-key-global :util :popwin-close 'popwin:close-popup-window)
    (user/bind-key-global :util :popwin-buffer 'popwin:popup-buffer)
    :config
    (popwin-mode t)))

(user--popups-config)


(provide 'ux/popups)
;;; popups.el ends here
