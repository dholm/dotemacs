;;; rust.el --- Rust mode support
;;; Commentary:
;;; Code:

(defun user/rust-mode-hook ()
  "Rust mode hook."
  (with-feature 'rusti
    (rusti-minor-mode t)))


(defun user/rust-mode-init ()
  "Initialize Rust mode."
  ;;; (Hooks) ;;;
  (add-hook 'rust-mode-hook 'user/rust-mode-hook))

(with-executable 'rustc
  (req-package rust-mode
    :config (user/rust-mode-init))
  (req-package flycheck-rust)
  (req-package rusti
    :loader :el-get))


(provide 'modes/rust)
;;; rust.el ends here
