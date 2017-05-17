;;; rust.el --- Rust mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--rust-mode-hook ()
  "Rust mode hook."
  (with-feature 'rusti
    (rusti-minor-mode t)))

(use-package rust-mode
  :if (executable-find "rustc")
  :defer
  :init
  (add-hook 'rust-mode-hook 'user--rust-mode-hook)
  :config
    ;;; (Packages) ;;;
  (use-package flycheck-rust)
  (use-package rusti
    :quelpa (rusti
             :fetcher github
             :repo "ruediger/rusti.el")))


(provide 'modes/rust)
;;; rust.el ends here
