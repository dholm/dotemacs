;;; php.el --- initializes PHP modes
;;; Commentary:
;;; Code:

(defun user/php-mode-hook ()
  "PHP mode hook."
  ;; Bring in CEDET.
  (with-feature 'cedet
    (user/cedet-hook)
    (when (derived-mode-p 'web-mode)
      ;; In web-mode CEDET doesn't automatically load PHP support.
      (wisent-php-default-setup)))

  ;; Separate camel-case into separate words
  (subword-mode t)

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.php")))


(defun user/php-mode-init ()
  "Initialize PHP mode."
  (after-load 'web-mode
    (user/add-web-mode-hook 'php 'user/php-mode-hook)))

(with-executable 'php
  (user/php-mode-init))


(provide 'modes/php)
;;; php.el ends here
