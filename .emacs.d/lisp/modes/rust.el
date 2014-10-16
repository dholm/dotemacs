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
  (require-package '(:name rust-mode :after (user/rust-mode-init)))
  (require-package '(:name rusti)))


(provide 'modes/rust)
;;; rust.el ends here
