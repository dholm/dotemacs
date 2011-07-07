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
(setq load-path (cons "~/.emacs.d/vendor/google-c-style" load-path))
(require 'google-c-style)


;; Enable installed helpers for C/C++
(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Run spell-checker on strings and comments
	    (flyspell-prog-mode)
	    ;; Separate camel-case into separate words
	    (subword-mode t)
	    (when (featurep 'cedet)
	      ;; Use semantic as a source for auto complete
	      (setq ac-sources (append ac-sources '(ac-source-semantic))))
	    (when (featurep 'dtrt-indent)
	      ;; Enable dtrt-indent to attempt to identify the indentation rules used
	      (dtrt-indent-mode t))))


;; (Text Conventions) ;;

;; When using fill-paragraph or auto-fill-mode break lines at 80 characters
(setq-default fill-column 80)
