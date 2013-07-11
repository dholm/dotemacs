
;; Bootstrap Emacs and load benchmarking
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'exec-path (concat user-emacs-directory "bin"))
(require 'utilities/benchmarking)


;; Set up global constants
(require 'utilities/path)
(defconst *user-home-directory* (concat (expand-file-name "~") "/"))
(defconst *user-cache-directory* (path-join *user-home-directory* ".emacs.cache"))
(defconst *user-el-get-directory* (path-join user-emacs-directory "el-get"))


;; Configure ELPA repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))


;; Configure and load el-get
(add-to-list 'load-path (path-join *user-el-get-directory* "el-get"))

(setq el-get-user-package-directory (path-join user-emacs-directory "init")
      el-get-sources
      '(;; Code helpers
        auto-complete clang-complete-async cedet dtrt-indent ecb google-c-style
        smart-tab ensime scion pymacs jedi pde perl-completion yasnippet
        auto-complete-yasnippet deferred-flyspell flymake-cursor flycheck
        auto-complete-emacs-lisp redshank c-eldoc

        ;; Modes
        gnuplot-mode haskell-mode js2-mode markdown-mode nxhtml rainbow-mode
        wc-mode python-mode pylookup slime php-mode scala-mode2 cperl-mode
        jinja showcss-mode newlisp-mode swank-newlisp ttcn-mode skewer-mode

        ;; Version Control Systems
        magit vc-clearcase git-gutter-fringe

        ;; Utilities
        deft sunrise-commander elim perspective shell-command bash-completion
        emacs-w3m bm helm helm-descbinds helm-etags-plus auto-compile
        helm-build-command helm-ls-git helm-c-yasnippet

        ;; Editing
        multiple-cursors expand-region undo-tree browse-kill-ring paredit
        fill-column-indicator elisp-slime-nav rainbow-delimiters

        ;; Navigation
        ace-jump-mode jump-char minimap smart-forward

        ;; Themes
        solarized-theme))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync el-get-sources)


;; Load additional configuration
(load "emacs.el")
(load "bindings.el")
(load "themes.el")
(load "modes.el")
(load "utilities.el")


;; If ~/.emacs.local is available load it as the last file so that it is
;; possible to add local settings and overrides.
(setq user-local-init (path-join *user-home-directory* ".emacs.local"))
(when (file-exists-p user-local-init)
  (load user-local-init))
