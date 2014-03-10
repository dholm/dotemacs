;;; php.el --- initializes PHP modes
;;; Commentary:
;;; Code:

(defun user/php-mode-hook ()
  "PHP mode hook."
  ;; Separate camel-case into separate words
  (subword-mode t)

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.php")))


(defun user/php-mode-init ()
  "Initialize PHP mode."
  (add-hook 'php-mode-hook 'user/php-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name php-mode :after (user/php-mode-init))))

(with-executable 'php
  (user/php-mode-init))


(provide 'modes/php)
;;; php.el ends here
