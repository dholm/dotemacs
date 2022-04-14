;;; rust.el --- Rust mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package rustic
  :if (executable-find "rustc")
  :defer
  :mode ("\\.rs$" . rustic-mode))


(provide 'modes/rust)
;;; rust.el ends here
