;;; wc-mode --- word count in modeline
;;; Commentary:
;;; Code:

(defun dholm/wc-mode-init ()
  "Initialize wc-mode."
  (define-key dholm/utilities-map (kbd "w") 'wc-mode))

(require-package '(:name wc-mode :after (dholm/wc-mode-init)))


(provide 'utilities/wc-mode)
;;; wc-mode.el ends here
