;;; prog --- setup shared defaults for programming modes
;;; Commentary:
;;; Code:

(defun user/prog-mode-hook ()
  "Programming mode hook."
  (setq-default
   ;; Indent using spaces by default
   indent-tabs-mode nil)
  ;; Run spell-checker
  (flyspell-prog-mode)
  ;; Delete trailing whitespace on save
  (add-hook 'write-contents-functions 'delete-trailing-whitespace nil t)
  ;; Enable dtrt-indent to attempt to identify the indentation rules used
  (after-load 'dtrt-indent
    (dtrt-indent-mode t)))

(add-hook 'prog-mode-hook 'user/prog-mode-hook)


(provide 'modes/prog)
;;; prog.el ends here
