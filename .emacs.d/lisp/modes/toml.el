;;; toml.el --- Initializes TOML mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--toml-mode-hook ()
  "Mode hook for TOML buffers."
  (unless (derived-mode-p 'toml-mode)
    (user--prog-mode-hook)))


(use-package toml-mode
  :config
  (add-hook 'toml-mode-hook 'user--toml-mode-hook))


(provide 'modes/toml)
;;; toml.el ends here
