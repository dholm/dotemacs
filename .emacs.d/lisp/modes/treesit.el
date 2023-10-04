;;; treesit.el --- Treesitter mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treesit
  :ensure nil
  :if (and (version<= "29.1" emacs-version)
           (treesit-available-p))
  :config
  (validate-setq
   treesit-language-source-alist
   '((awk "https://github.com/Beaglefoot/tree-sitter-awk")
     (bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (c++ "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (use-package treesit-auto
    :config
    (validate-setq
     ;; Automatically install missing grammars.
     treesit-auto-install t)

    (global-treesit-auto-mode)))


(provide 'modes/treesit)
;;; treesit.el ends here
