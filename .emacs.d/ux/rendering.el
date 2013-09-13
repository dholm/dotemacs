;;; rendering.el --- Configure Emacs user interface rendering
;;; Commentary:
;;; Code:

(defun user/rendering-init ()
  "Setup Emacs user interface rendering."
  (setq-default
   ;; Update Emacs display every ten seconds when idle.
   idle-update-delay 10
   ;; Redraw the entire screen before checking for pending input events.
   ;; This will improve performance in general but might degrade performance of
   ;; key repeat.
   redisplay-dont-pause t))


(user/rendering-init)


(provide 'ux/rendering)
;;; rendering.el ends here