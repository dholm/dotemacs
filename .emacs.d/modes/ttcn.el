;;; ttcn --- initializes TTCN modes
;;; Commentary:
;;; Code:

(require-package '(:name ttcn-mode
			 :type github
			 :pkgname "dholm/ttcn-el"
			 :post-init (progn
				      (add-to-list 'auto-mode-alist '("\\.mp$" . ttcn-mode))
				      (add-to-list 'auto-mode-alist '("\\.ttcn" . ttcn-3-mode)))))


;; Set the TTCN-3 mode hook
(defun dholm/ttcn3-mode-hook ()
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode)
  ;; Separate camel-case into separate words
  (subword-mode t)
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t)
  ;; Enable dtrt-indent to attempt to identify the indentation rules used
  (dtrt-indent-mode t))

(add-hook 'ttcn3-mode-hook 'dholm/ttcn3-mode-hook)


(provide 'modes/ttcn)
;;; ttcn.el ends here
