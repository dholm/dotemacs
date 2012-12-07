;; (Code Conventions) ;;

;; Set the default C/C++ code style
(setq c-default-style "K&R")
(setq c++-default-style "Stroustrup")

;; Override the indentation level of case labels in the K&R- and Stroustrup
;; styles so that they are indented one level beyond the switch.
(add-hook 'c-mode-common-hook
      (lambda ()
        (c-set-offset 'case-label '+)))


;; Load the Google C/C++ style
(setq load-path (cons "~/.emacs.d/vendor/google-styleguide" load-path))
(require 'google-c-style)


;; Enable installed helpers for C/C++
(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Run spell-checker on strings and comments
	    (flyspell-prog-mode)
	    ;; Separate camel-case into separate words
	    (subword-mode t)
	    ;; Show trailing whitespace
	    (setq show-trailing-whitespace t)
	    (add-hook 'before-save-hook
		      ;; Delete trailing whitespace on save
		      'delete-trailing-whitespace nil t)
	    (when (featurep 'dtrt-indent)
	      ;; Enable dtrt-indent to attempt to identify the indentation rules used
	      (dtrt-indent-mode t))))


;; Use gdb-script-mode for files ending in .gdb
(setq auto-mode-alist
      (cons '("\\.gdb$" . gdb-script-mode) auto-mode-alist))


;; TTCN Conventions
;; Enables support for TTCN-3
(setq load-path (cons "~/.emacs.d/vendor/ttcn-el" load-path))
(require 'ttcn3)
(setq auto-mode-alist
      (cons '("\\.ttcn" . ttcn-3-mode) auto-mode-alist))


;; Set the TTCN-3 mode hook
(add-hook 'ttcn3-mode-hook
	  (lambda ()
	    ;; Run spell-checker on strings and comments
	    (flyspell-prog-mode)
	    ;; Separate camel-case into separate words
	    (subword-mode t)
	    ;; Show trailing whitespace
	    (setq show-trailing-whitespace t)
	    (add-hook 'before-save-hook
		      ;; Delete trailing whitespace on save
		      'delete-trailing-whitespace nil t)
	    (when (featurep 'dtrt-indent)
	      ;; Enable dtrt-indent to attempt to identify the indentation rules used
	      (dtrt-indent-mode t))))


;; Markdown Mode
(setq load-path (cons "~/.emacs.d/vendor/markdown-mode" load-path))
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.text$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.markdown$" . markdown-mode) auto-mode-alist))


;; (Text Conventions) ;;

;; When using fill-paragraph or auto-fill-mode break lines at 80 characters
(setq-default fill-column 80)


;; Prefer UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
