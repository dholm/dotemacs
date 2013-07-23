;;; php --- initializes PHP modes
;;; Commentary:
;;; Code:

(require-package '(:name php-mode))


;; Set up helpers for php-mode
(defun dholm/php-mode-hook ()
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

(add-hook 'php-mode-hook 'dholm/php-mode-hook)


(provide 'modes/php)
;;; php.el ends here
