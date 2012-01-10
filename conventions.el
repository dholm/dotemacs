;; (Code Conventions) ;;

;; Set the default C/C++ code style
(setq c-default-style "K&R")
(setq c++-default-style "Stroustrup")

;; Override the indentation level of case labels in the K&R- and Stroustrup
;; styles so that they are indented one level beyond the switch.
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-set-offset 'case-label '+)))


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


;; Enable installed helpers for Python
(add-hook 'python-mode-hook
	  (lambda ()
	    ;; Run spell-checker on strings and comments
	    (flyspell-prog-mode)
	    ;; Separate camel-case into separate words
	    (subword-mode t)
	    ;; Show trailing whitespace
	    (setq show-trailing-whitespace t)
	    (add-hook 'before-save-hook
		      ;; Delete trailing whitespace on save
		      'delete-trailing-whitespace nil t)))


;; (Text Conventions) ;;

;; When using fill-paragraph or auto-fill-mode break lines at 80 characters
(setq-default fill-column 80)
