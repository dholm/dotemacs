;;; rust.el --- Rust mode support
;;; Commentary:
;;; Code:

(defun user--rust-mode-hook ()
  "Rust mode hook."
  (with-feature 'rusti
    (rusti-minor-mode t)))


(with-executable 'rustc
  (use-package rust-mode
    :config
    ;;; (Hooks) ;;;
    (add-hook 'rust-mode-hook 'user--rust-mode-hook)

    ;;; (Packages) ;;;
    (use-package flycheck-rust)
    (use-package rusti
      :quelpa (rusti
               :fetcher github
               :repo "ruediger/rusti.el"))))


(provide 'modes/rust)
;;; rust.el ends here
