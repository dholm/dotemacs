;;; popups.el --- Configure Emacs popups
;;; Commentary:
;;; Code:

(defun user--popup-config ()
  "Initialize popup."
  ;; Install workaround for whitespace-mode bug.
  (after-load 'modes/whitespace
    (defadvice popup-draw (before user/turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing popup."
      (user/whitespace-mode-suppress t))

    (defadvice popup-delete (after user/restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting popup."
      (user/whitespace-mode-suppress nil))))


(defun user--popwin-config ()
  "Initialize popwin."
  (with-feature 'popwin
    (popwin-mode t)

    ;;; (Bindings) ;;;
    (user/bind-key-global :util :popwin-close 'popwin:close-popup-window)
    (user/bind-key-global :util :popwin-buffer 'popwin:popup-buffer)))


(defun user--popups-config ()
  "Initialize Emacs popups."
  (validate-setq
   ;; Timeout for messages shown in minibuffer.
   minibuffer-message-timeout 5)

  ;;; (Packages) ;;;
  (use-package popup
    :ensure t
    :config (user--popup-config))
  (use-package popwin
    :ensure t
    :config (user--popwin-config)))

(user--popups-config)


(provide 'ux/popups)
;;; popups.el ends here
