;; CSS editing
(defun dholm/css-mode-hook ()
  (rainbow-mode t)
  (set (make-local-variable 'ac-auto-start) 2)
  (set (make-local-variable 'ac-auto-show-menu) t))
(add-hook 'css-mode-hook 'dholm/css-mode-hook)
