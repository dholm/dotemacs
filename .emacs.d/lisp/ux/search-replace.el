;;; search-replace.el --- Configuration for searching and replacing
;;; Commentary:
;;; Code:

(defun user/visual-regexp-init ()
  "Initialize visual regexp."
  ;;; (Bindings) ;;;
  (global-set-key [remap query-replace-regexp] 'vr/query-replace)
  (global-set-key [remap replace-regexp] 'vr/replace))


(defun user/anzu-init ()
  "Initialize anzu."
  (require 'anzu)
  (global-anzu-mode t)
  (after-load 'diminish
    (diminish 'anzu-mode)))


(defun user/search-replace-init ()
  "Initialize Emacs search and replace."
  (setq-default
   ;; Highlight matches when using grep.
   grep-highlight-matches t
   ;; Highlight all visible matches.
   search-highlight t
   query-replace-highlight t
   ;; Perform certain commands only on the marked region.
   transient-mark-mode t)

  ;;; (Bindings) ;;;
  ;; Use regular expression searches by default.
  (user/bind-key-global :basic :search-forward 'isearch-forward-regexp)
  (user/bind-key-global :basic :search-backward 'isearch-backward-regexp)
  (user/bind-key-global :basic :search-files 'find-grep)

  ;;; (Packages) ;;;
  (require-package '(:name visual-regexp :after (user/visual-regexp-init)))
  (require-package '(:name anzu :after (user/anzu-init))))

(user/search-replace-init)


(provide 'ux/search-replace)
;;; search-replace.el ends here
