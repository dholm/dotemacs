;;; prog --- setup shared defaults for programming modes
;;; Commentary:
;;; Code:

(defun dholm/prog-mode-hook ()
  "Programming mode hook."
  (setq-default
   ;; Indent using spaces by default
   indent-tabs-mode nil)
  ;; Run spell-checker
  (flyspell-prog-mode)
  ;; Delete trailing whitespace on save
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  ;; Enable dtrt-indent to attempt to identify the indentation rules used
  (after-load 'dtrt-indent
    (dtrt-indent-mode t)))

(add-hook 'prog-mode-hook 'dholm/prog-mode-hook)


(provide 'modes/prog)
;;; prog.el ends here
