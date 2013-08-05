;;; init-modes --- initializes major modes
;;; Commentary:
;;; Code:

;; Prefer UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


(require 'modes/c-c++)
(require 'modes/csv)
(require 'modes/gdb)
(require 'modes/gnuplot)
(require 'modes/go)
(require 'modes/haskell)
(require 'modes/html)
(require 'modes/javascript)
(require 'modes/jinja)
(require 'modes/lisp)
(require 'modes/makefile)
(require 'modes/markdown)
(require 'modes/perl)
(require 'modes/php)
(require 'modes/prog)
(require 'modes/python)
(require 'modes/rest)
(require 'modes/ruby)
(require 'modes/scala)
(require 'modes/shell)
(require 'modes/syslog)
(require 'modes/text)
(require 'modes/ttcn)
(require 'modes/whitespace)
(require 'modes/xml)


(provide 'init-modes)
;;; init-modes.el ends here
