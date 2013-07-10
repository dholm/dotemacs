;; (Code Conventions) ;;

(require 'modes/lisp)
(require 'modes/c-c++)
(require 'modes/makefile)
(require 'modes/shell)
(require 'modes/python)
(require 'modes/gdb)
(require 'modes/web)
(require 'modes/gnuplot)
(require 'modes/javascript)
(require 'modes/haskell)
(require 'modes/ttcn)
(require 'modes/php)
(require 'modes/scala)
(require 'modes/perl)


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


;; Enable whitespace mode
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(autoload 'whitespace-toggle-options "whitespace" "Toggle local whitespace-mode options." t)
(setq whitespace-style '(trailing lines space-before-tab indentation space-after-tab)
      whitespace-line-column 400)
