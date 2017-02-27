;;; introspection.el --- Emacs introspection -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/find-symbol-containing-regex (search-term)
  "Print all symbols containing SEARCH-TERM."
  (interactive (list (read-from-minibuffer "Search string: ")))
  (mapatoms (lambda (symbol)
              (when (boundp (intern (symbol-name symbol)))
                (when (and (stringp (symbol-value symbol))
                           (string-match search-term (symbol-value symbol)))
                  (message "%s=[%s]" (symbol-name symbol) (symbol-value symbol)))))))



(provide 'lib/introspection)
;;; introspection.el ends here
