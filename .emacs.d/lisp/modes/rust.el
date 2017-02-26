;;; rust.el --- Rust mode support
;;; Commentary:
;;; Code:

(defun user--rust-mode-hook ()
  "Rust mode hook."
  (with-feature 'rusti
    (rusti-minor-mode t)))

(with-executable 'rustc
  (use-package rust-mode
    :defer
    :init
    (add-hook 'rust-mode-hook 'user--rust-mode-hook)
    :config
    ;;; (Packages) ;;;
    (use-package flycheck-rust)
    (use-package rusti
      :quelpa (rusti
               :fetcher github
               :repo "ruediger/rusti.el"))))


(provide 'modes/rust)
;;; rust.el ends here
