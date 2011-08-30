;; (Utilities) ;;

;; CEDET
(setq load-path (cons "~/.emacs.d/vendor/cedet/common" load-path))
(if (file-readable-p (expand-file-name "~/.emacs.d/vendor/cedet/common/cedet.elc"))
    (unless (featurep 'cedet)
      (require 'cedet)))
(when (featurep 'cedet)
  (global-ede-mode 1)
  (setq semantic-clang-binary "clang")
  (semantic-load-enable-minimum-features)
  (semantic-load-enable-code-helpers)
  (semantic-load-enable-gaudy-code-helpers)
  (semantic-load-enable-primary-exuberent-ctags-support)
  (global-semanticdb-minor-mode 1)
  (global-srecode-minor-mode 1)
  (setq semanticdb-default-save-directory "~/.emacs.cache/semanticdb"))


;; Yet Another Snippet extension
(setq load-path (cons "~/.emacs.d/vendor/yasnippet" load-path))
(require 'yasnippet)
(when (featurep 'yasnippet)
  (setq yas/snippet-dirs "~/.emacs.d/vendor/yasnippet/snippets")
  (yas/initialize)
  (yas/load-directory yas/snippet-dirs))


;; auto-complete-mode offers superior code completion over existing tools
(setq load-path (cons "~/.emacs.d/vendor/auto-complete" load-path))
(require 'auto-complete-config)
(when (featurep 'auto-complete)
  (setq load-path (cons "~/.emacs.d/vendor/auto-complete-clang" load-path))
  (require 'auto-complete-clang)
  (when (featurep 'auto-complete-clang)
    (add-hook 'c-mode-common-hook
	      (lambda ()
		(setq ac-sources (append '(ac-source-clang) ac-sources)))))

  (ac-config-default)
  (setq ac-auto-start nil)
  (setq ac-quick-help-delay 0.5)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/ac-dict")
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary
					      ac-source-words-in-buffer))
  (when (featurep 'cedet)
    ;; Use semantic as a source for auto complete
    (setq ac-sources '(ac-source-semantic)))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (setq ac-sources (append '(ac-source-yasnippet) ac-sources))))
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  ;; Store the completion history in the cache directory
  (setq ac-comphist-file "~/.emacs.cache/ac-comphist.dat")
  (global-auto-complete-mode t))


;; XCscope
(setq load-path (cons "~/.emacs.d/vendor/xcscope" load-path))
(require 'xcscope)


;; Enable automatic detection of indentation style
(setq load-path (cons "~/.emacs.d/vendor/dtrt-indent" load-path))
(require 'dtrt-indent)


;; Close all open buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;; Show row and column numbers
(setq line-number-mode t)
(setq column-number-mode t)


;; Browsable kill ring
(setq load-path (cons "~/.emacs.d/vendor/browse-kill-ring" load-path))
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))


;; Magit advanced Git integration
(setq load-path (cons "~/.emacs.d/vendor/magit" load-path))
(require 'magit)


;; SmartTab intelligent tab completion control
(setq load-path (cons "~/.emacs.d/vendor/smart-tab" load-path))
(require 'smart-tab)
(when (featurep 'smart-tab)
  (global-smart-tab-mode))


;; If running on windows load Outlook Edit
(setq load-path (cons "~/.emacs.d/vendor/outlookedit" load-path))
(if (eq system-type 'windows-nt)
    (require 'outlookedit))


;; LustyExplorer
(setq load-path (cons "~/.emacs.d/vendor/lusty-emacs" load-path))
(require 'lusty-explorer)
(when (featurep 'lusty-explorer)
  (global-set-key "\C-x\C-f" 'lusty-file-explorer)
  (global-set-key "\C-xb" 'lusty-buffer-explorer))


;; Wind Move enables window navigation
(require 'windmove)
(when (featurep 'windmove)
  (windmove-default-keybindings))


;; Visualize undo history as a tree structure
(setq load-path (cons "~/.emacs.d/vendor/undo-tree" load-path))
(require 'undo-tree)
(when (featurep 'undo-tree)
  (global-undo-tree-mode))


;; MultiTerm terminal
(setq load-path (cons "~/.emacs.d/vendor/multi-term" load-path))
(require 'multi-term)


;; Make buffer names unique
(require 'uniquify)
(when (featurep 'uniquify)
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"))


;; Enable TabBar minor mode
(setq load-path (cons "~/.emacs.d/vendor/tabbar" load-path))
(require 'tabbar)
(when (featurep 'tabbar-mode)
  (tabbar-mode))


;; Save/restore the position in Emacs buffers between sessions
(require 'saveplace)
(when (featurep 'saveplace)
  (setq save-place-file "~/.emacs.cache/saveplace")
  (setq-default save-place t))


;; Save/restore the history of various Emacs minibuffers
(require 'savehist)
(when (featurep 'savehist)
  (setq savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
  (setq savehist-file "~/.emacs.cache/savehist")
  (savehist-mode t))


;; When using profile-dotemacs start with init.el
;; To profile run emacs (-Q) -l ~/.emacs.d/vendor/profile-dotemacs/profile-dotemacs.el -f profile-dotemacs
(setq profile-dotemacs-file "~/.emacs.d/init.el")


;; When using gud-mode to debug enable gdb-many-windows and a separate IO buffer
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
