;;; rust.el --- Rust mode support
;;; Commentary:
;;; Code:

(defun user--rust-mode-hook ()
  "Rust mode hook."
  (with-feature 'rusti
    (rusti-minor-mode t)))


(defun user--rust-mode-config ()
  "Initialize Rust mode."
  ;;; (Hooks) ;;;
  (add-hook 'rust-mode-hook 'user--rust-mode-hook))

(with-executable 'rustc
  (use-package rust-mode
    :defer t
    :config (user--rust-mode-config))
  (use-package flycheck-rust
    :defer t)
  (require-package '(:name rusti)))


(provide 'modes/rust)
;;; rust.el ends here
