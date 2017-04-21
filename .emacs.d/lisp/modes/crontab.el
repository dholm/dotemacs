;;; crontab.el --- Configure crontab mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package crontab-mode
  :defer
  :mode (("\\.?cron\\(tab\\)?\\'" . crontab-mode)))


(provide 'modes/crontab)
;;; crontab.el ends here
