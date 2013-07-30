;;; php --- initializes PHP modes
;;; Commentary:
;;; Code:

(defun dholm/php-mode-hook ()
  "PHP mode hook."
  ;; Separate camel-case into separate words
  (subword-mode t))


(defun dholm/php-mode-init ()
  "Initialize PHP mode."
  (add-hook 'php-mode-hook 'dholm/php-mode-hook))

(require-package '(:name php-mode :after (dholm/php-mode-init)))


(provide 'modes/php)
;;; php.el ends here
