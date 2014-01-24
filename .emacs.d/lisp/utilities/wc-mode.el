;;; wc-mode --- word count in modeline
;;; Commentary:
;;; Code:

(defun user/wc-mode-init ()
  "Initialize wc-mode."
  (define-key user/utilities-map (kbd "w") 'wc-mode))

(require-package '(:name wc-mode :after (user/wc-mode-init)))


(provide 'utilities/wc-mode)
;;; wc-mode.el ends here
