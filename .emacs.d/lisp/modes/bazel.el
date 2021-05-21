;;; bazel.el --- Bazel mode configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package bazel
  :if (executable-find "bazel")
  :defer)


(provide 'modes/bazel)
;;; bazel.el ends here
