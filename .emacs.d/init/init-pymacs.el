;; Ropemacs Python refactoring library
(add-hook 'python-mode-hook (lambda ()
			      (pymacs-load "ropemacs" "rope-")))
(setq ropemacs-enable-autoimport t)
