;;; ttcn.el --- initializes TTCN modes
;;; Commentary:
;;; Code:

(defun user/ttcn3-mode-hook ()
  "TTCN mode hook."
  ;; Separate camel-case into separate words
  (subword-mode t))

(defun user/ttcn-mode-init ()
  "Initialize TTCN mode."
  (add-hook 'ttcn3-mode-hook 'user/ttcn3-mode-hook))

(require-package '(:name ttcn-mode :after (user/ttcn-mode-init)))


(provide 'modes/ttcn)
;;; ttcn.el ends here
