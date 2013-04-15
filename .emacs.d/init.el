
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
        auto-complete auto-complete-clang cedet dtrt-indent ecb flymake-cursor
        google-c-style smart-tab ensime scion

        ;; Modes
        gnuplot-mode haskell-mode js2-mode markdown-mode multi-web-mode
        rainbow-mode wc-mode pymacs python pylookup slime php-mode scala-mode2

        ;; Version Control Systems
        magit vc-clearcase

        ;; Utilities
        deft undo-tree lusty-explorer profile-dotemacs multi-term
        browse-kill-ring sunrise-commander elim

        ;; Themes
        solarized-theme

        ;; Miscellaneous
        vlf))
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
