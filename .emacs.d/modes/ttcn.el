;;; ttcn --- initializes TTCN modes
;;; Commentary:
;;; Code:

(defun dholm/ttcn3-mode-hook ()
  "TTCN mode hook."
  ;; Separate camel-case into separate words
  (subword-mode t))


(defun dholm/ttcn-mode-init ()
  "Initialize TTCN mode."
  (add-hook 'ttcn3-mode-hook 'dholm/ttcn3-mode-hook))

(require-package '(:name ttcn-mode
			 :type github
			 :pkgname "dholm/ttcn-el"
			 :post-init (progn
				      (add-to-list 'auto-mode-alist '("\\.mp$" . ttcn-mode))
				      (add-to-list 'auto-mode-alist '("\\.ttcn" . ttcn-3-mode)))
                         :after (dholm/ttcn-mode-init)))


(provide 'modes/ttcn)
;;; ttcn.el ends here
