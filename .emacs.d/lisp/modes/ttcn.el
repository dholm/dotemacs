;;; ttcn.el --- initializes TTCN modes
;;; Commentary:
;;; Code:

(defun user--ttcn3-mode-hook ()
  "TTCN mode hook."
  ;; Separate camel-case into separate words
  (subword-mode t))

(use-package ttcn-mode
  :defer
  :quelpa (ttcn-mode
           :fetcher github
           :repo "dholm/ttcn-el")
  :init
  (add-auto-mode 'ttcn-mode "\\.mp$")
  (add-auto-mode 'ttcn-3-mode "\\.ttcn")
  :config
  (add-hook 'ttcn3-mode-hook 'user--ttcn3-mode-hook))


(provide 'modes/ttcn)
;;; ttcn.el ends here
