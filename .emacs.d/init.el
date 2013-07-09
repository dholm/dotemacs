
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-user-package-directory "~/.emacs.d/init")
(setq el-get-sources
      '(;; Code helpers
        auto-complete clang-complete-async cedet dtrt-indent ecb google-c-style
        smart-tab ensime scion pymacs jedi pde perl-completion yasnippet
        auto-complete-yasnippet flymake-cursor flycheck

        ;; Modes
        gnuplot-mode haskell-mode js2-mode markdown-mode nxhtml rainbow-mode
        wc-mode python-mode pylookup slime php-mode scala-mode2 cperl-mode
        jinja showcss-mode newlisp-mode swank-newlisp ttcn-mode

        ;; Version Control Systems
        magit vc-clearcase git-gutter-fringe

        ;; Utilities
        deft profile-dotemacs sunrise-commander elim perspective shell-command
        bash-completion emacs-w3m bm helm helm-descbinds helm-etags-plus
        helm-build-command helm-ls-git helm-ipython helm-c-yasnippet

        ;; Editing
        multiple-cursors expand-region undo-tree browse-kill-ring
        fill-column-indicator

        ;; Navigation
        ace-jump-mode jump-char minimap smart-forward

        ;; Themes
        solarized-theme))
(el-get 'sync el-get-sources)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

(push "~/.emacs.d" load-path)
(push "~/.emacs.d/bin" exec-path)

(load "emacs.el")
(load "themes.el")
(load "modes.el")
(load "utilities.el")
(load "bindings.el")

;; If ~/.emacs.local is available load it as the last file so that it is
;; possible to add local settings and overrides.
(if (file-readable-p (expand-file-name "~/.emacs.local"))
    (load-file (expand-file-name "~/.emacs.local"))
  nil)
