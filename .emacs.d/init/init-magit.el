;;; (Initialization) ;;;

;; Magit advanced Git integration
(global-set-key (kbd "C-c m") 'magit-status)


;; Full screen magit status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (when (get-register :magit-fullscreen)
    (ignore-errors
      (jump-to-register :magit-fullscreen))))
