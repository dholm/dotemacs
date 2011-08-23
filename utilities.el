;; (Utilities) ;;

;; CEDET
(setq load-path (cons "~/.emacs.d/vendor/cedet/common" load-path))
(if (file-readable-p (expand-file-name "~/.emacs.d/vendor/cedet/common/cedet.elc"))
    (unless (featurep 'cedet)
      (require 'cedet)
      (when (featurep 'cedet)
	(global-ede-mode 1)
	(setq semantic-clang-binary "clang")
	(semantic-load-enable-minimum-features)
	(semantic-load-enable-code-helpers)
	(semantic-load-enable-gaudy-code-helpers)
	(semantic-load-enable-primary-exuberent-ctags-support)
	(global-srecode-minor-mode 1)
	(setq semanticdb-default-save-directory "~/.emacs.cache/semanticdb"))))


;; auto-complete-mode offers superior code completion over existing tools
(setq load-path (cons "~/.emacs.d/vendor/auto-complete" load-path))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/ac-dict")
(ac-config-default)
;; Store the completion history in the cache directory
(setq ac-comphist-file "~/.emacs.cache/ac-comphist.dat")


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


;; Yet Another Snippet extension
(setq load-path (cons "~/.emacs.d/vendor/yasnippet" load-path))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")


;; Enable both HippieCompletion and indent for tab
(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (dabbrev-expand nil)
    (indent-for-tab-command)))


;; Tab completion
(let (modelist it)
  (setq modelist '(
		   'c-mode-common-hook
		   'lisp-mode-common-hook
		   'text-mode-hook
		   'java-mode-hook
		   'emacs-lisp-mode-hook
		   'LaTex-mode-hook
		   'Tex-Mode-hook
		   'python-mode-hook
		   'octave-mode-hook))
  (setq it modelist)
  (while it
    (add-hook (eval (car it)) (function
			       (lambda ()
				 (local-set-key (kbd "<tab>") 'indent-or-complete))))
    (add-hook (eval (car it)) (function
			       (lambda ()
				 (local-set-key (kbd "TAB") 'indent-or-complete))))
    (setq it (cdr it))))


;; The order that different completes are tested.
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev-visible
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-expand-all-abbrevs
	try-expand-line))


;; If running on windows load Outlook Edit
(setq load-path (cons "~/.emacs.d/vendor/outlookedit" load-path))
(if (eq system-type 'windows-nt)
    (require 'outlookedit))


;; LustyExplorer
(setq load-path (cons "~/.emacs.d/vendor/lusty-emacs" load-path))
(require 'lusty-explorer)
(global-set-key "\C-x\C-f" 'lusty-file-explorer)
(global-set-key "\C-xb" 'lusty-buffer-explorer)


;; Wind Move enables window navigation
(require 'windmove)
(windmove-default-keybindings 'super)
(global-set-key (kbd "<C-s-left>") 'windmove-left)
(global-set-key (kbd "<C-s-right>") 'windmove-right)
(global-set-key (kbd "<C-s-up>") 'windmove-up)
(global-set-key (kbd "<C-s-down>") 'windmove-down)


;; Visualize undo history as a tree structure
(setq load-path (cons "~/.emacs.d/vendor/undo-tree" load-path))
(require 'undo-tree)
(global-undo-tree-mode)


;; MultiTerm terminal
(setq load-path (cons "~/.emacs.d/vendor/multi-term" load-path))
(require 'multi-term)
