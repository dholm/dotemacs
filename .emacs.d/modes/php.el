;;; php.el --- initializes PHP modes
;;; Commentary:
;;; Code:

(defconst *has-php* (executable-find "php"))


(defun user/php-mode-hook ()
  "PHP mode hook."
  ;; Separate camel-case into separate words
  (subword-mode t)

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.php")))


(defun user/php-mode-init ()
  "Initialize PHP mode."
  (require-package '(:name php-mode :after (user/php-mode-init)))

  (add-hook 'php-mode-hook 'user/php-mode-hook))

(when *has-php*
  (user/php-mode-init))


(provide 'modes/php)
;;; php.el ends here
