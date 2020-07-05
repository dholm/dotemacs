;;; eldoc.el --- Show docstrings in echo area -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eldoc
  :diminish eldoc-mode
  :config
  (use-package eldoc-eval
    :config
    (eldoc-in-minibuffer-mode t)))


(provide 'utilities/eldoc)
;;; eldoc.el ends here
