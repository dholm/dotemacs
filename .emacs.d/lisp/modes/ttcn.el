;;; ttcn.el --- initializes TTCN modes -*- lexical-binding: t; -*-
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
  :mode (("\\.mp$" . ttcn-mode)
         ("\\.ttcn" . ttcn-3-mode))
  :config
  (add-hook 'ttcn3-mode-hook 'user--ttcn3-mode-hook))


(provide 'modes/ttcn)
;;; ttcn.el ends here
