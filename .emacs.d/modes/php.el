;;; php --- initializes PHP modes
;;; Commentary:
;;; Code:

(defun user/php-mode-hook ()
  "PHP mode hook."
  ;; Separate camel-case into separate words
  (subword-mode t))


(defun user/php-mode-init ()
  "Initialize PHP mode."
  (add-hook 'php-mode-hook 'user/php-mode-hook))

(require-package '(:name php-mode :after (user/php-mode-init)))


(provide 'modes/php)
;;; php.el ends here
