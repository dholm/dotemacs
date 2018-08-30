;;; smartparens.el --- Set up smartparens. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/smartparens-enable ()
  "Enable smartparens in current mode."
  (when (feature-p 'smartparens)
    (show-smartparens-mode t)
    (smartparens-mode t)))

(defun user--sp-org-point-in-checkbox-p (_id action _context)
  "Check if smartparens ACTION is inside checkbox."
  (when (eq action 'insert)
    (sp--looking-at-p "\\s-*]")))

(defun user--sp-cxx-point-is-template-p (id action context)
  "Return t if point with ID using ACTION in CONTEXT is in the right place for C++ angle-brackets."
  (and (sp-in-code-p id action context)
       (sp-point-after-word-p id action context)))

(defun user--sp-cc-point-after-include-p (id action context)
  "Return t if point with ID using ACTION in CONTEXT is in an #include."
  (and (sp-in-code-p id action context)
       (save-excursion
         (goto-char (line-beginning-position))
         (looking-at-p "[       ]*#include[^<]+"))))

(defun user--sp-haskell-after-symbol-p (_id action _context)
  "Return t if point using ACTION is after a symbol."
  (when (eq action 'insert)
    (save-excursion
      (backward-char 1)
      (looking-back "\\sw\\|\\s_\\|\\s'"))))

(defun user--sp-gfm-skip-asterisk (_ms position _me)
  "Return t if asterisk should be skipped at point with POSITION."
  (save-excursion
    (goto-char position)
    (save-match-data (looking-at "^\\* "))))

(defun user--sp-php-handle-docstring (&rest _ignored)
  "Handle PHP docstrings with smartparens."
  (-when-let (line (save-excursion
                     (forward-line)
                     (thing-at-point 'line)))
    (cond
     ((string-match-p "function" line)
      (save-excursion
        (insert "\n")
        (let ((args (save-excursion
                      (forward-line)
                      (my-php-get-function-args))))
          (--each args
            (insert (format "* @param %s\n" it)))))
      (insert "* "))
     ((string-match-p ".*class\\|interface" line)
      (save-excursion (insert "\n*\n* @author\n"))
      (insert "* ")))
    (let ((o (sp--get-active-overlay)))
      (indent-region (overlay-start o) (overlay-end o)))))

(use-package smartparens
  :diminish smartparens-mode
  :bind-wrap
  (:map smartparens-mode-map
        ([remap kill-line] . sp-kill-hybrid-sexp)
        ((:key :basic :selection-next) . sp-select-next-thing-exchange)
        ((:key :basic :selection-prev) . sp-select-previous-thing)
        ((:key :basic :forward-word) . sp-forward-symbol)
        ((:key :basic :backward-word) . sp-backward-symbol)
        ((:key :basic :forward-expr) . sp-forward-sexp)
        ((:key :basic :backward-expr) . sp-backward-sexp)
        ((:key :code :unwrap-expr) . sp-unwrap-sexp)
        ((:key :code :comment) . sp-comment)
        ((:key :basic :cut-expr) . sp-kill-sexp)
        ((:key :basic :copy-expr) . sp-copy-sexp))
  :config
  (validate-setq
   ;; Don't kill trailing whitespace with `sp-hybrid-kill'.
   sp-hybrid-kill-excessive-whitespace nil)

  ;; Don't insert pairs automatically if point is at a word.
  (sp-pair "{" nil
           :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)
           :wrap "C-{")
  (sp-pair "(" nil
           :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)
           :wrap "C-(")
  (sp-pair "[" nil
           :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "\"" nil :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p))
  (sp-pair "'" nil :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p))

  (sp-with-modes '(c-mode c++-mode objc-mode)
    ;; Automatically add another newline before closing curly brace on enter.
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC") ("* ||\n[i]" "RET")))

    (sp-local-pair "<" ">" :when '(user--sp-cxx-point-is-template-p user--sp-cc-point-after-include-p))
    (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    ;; Doxygen blocks.
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  (sp-with-modes '(css-mode scss-mode less-css-mode stylus-mode)
    (sp-local-pair "/*" "*/" :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC"))))

  (sp-with-modes '(haskell-mode)
    (sp-local-pair "'" nil :unless '(user--sp-haskell-after-symbol-p))
    (sp-local-pair "\\(" nil :actions nil))

  (sp-with-modes '(javascript-mode js2-mode js3-mode rjsx-mode)
    (sp-local-pair "/* " " */" :post-handlers '(("| " "SPC"))))

  (sp-with-modes '(sh-mode markdown-mode gfm-mode rts-mode python-mode cython-mode)
    (sp-local-pair "`" nil :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p)))

  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*" :wrap "C-*" :skip-match 'user--sp-gfm-skip-asterisk)
    (sp-local-tag "2" "**" "**")
    (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-tag "i" "\"<" "\">")
    (sp-local-pair "$" nil :unless '(sp-point-before-word-p))
    (sp-local-pair "\\[" " \\]")
    (sp-local-pair "\\(" " \\)")
    (sp-local-pair "\\{" " \\}")
    (sp-local-pair "\\left(" " \\right)")
    (sp-local-pair "\\left\\{" " \\right\\}"))

  (sp-with-modes '(org-mode)
    (sp-local-pair "*" "*"
                   :post-handlers '(("[d1]" "SPC"))
                   :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-at-bol-p))
    (sp-local-pair "_" "_"
                   :post-handlers '(("[d1]" "SPC"))
                   :unless '(sp-point-after-word-p sp-point-before-word-p))
    (sp-local-pair "/" "/"
                   :post-handlers '(("[d1]" "SPC"))
                   :unless '(sp-point-after-word-p sp-point-before-word-p user--sp-org-point-in-checkbox-p))
    (sp-local-pair "~" "~"
                   :post-handlers '(("[d1]" "SPC"))
                   :unless '(sp-point-after-word-p sp-point-before-word-p))
    (sp-local-pair "=" "="
                   :post-handlers '(("[d1]" "SPC"))
                   :unless '(sp-point-after-word-p sp-point-before-word-p))
    (sp-local-pair "«" "»"))

  (sp-with-modes '(php-mode)
    (sp-local-pair "/**" "*/"
                   :post-handlers '(("| " "SPC") (user--sp-php-handle-docstring "RET")))
    (sp-local-pair "/*." ".*/" :post-handlers '(("| " "SPC")))
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

  (sp-with-modes '(scala-mode)
    (sp-local-pair "'" nil :actions nil)))


(provide 'utilities/smartparens)
;;; smartparens.el ends here
