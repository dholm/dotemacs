(require-package (:name ttcn-mode))


;; Set the TTCN-3 mode hook
(defun dholm/ttcn3-mode-hook ()
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode)
  ;; Separate camel-case into separate words
  (subword-mode t)
  ;; Show trailing whitespace
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t)
  ;; Enable dtrt-indent to attempt to identify the indentation rules used
  (dtrt-indent-mode t))

(add-hook 'ttcn3-mode-hook 'dholm/ttcn3-mode-hook)


(provide 'modes/ttcn)
