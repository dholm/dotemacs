;;; search-replace.el --- Configuration for searching and replacing
;;; Commentary:
;;; Code:

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
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  ;;; (Packages) ;;;
  (require-package '(:name visual-regexp :after (user/visual-regexp-init))))


(defun user/visual-regexp-init ()
  "Initialize visual regexp."
  ;;; (Bindings) ;;;
  (global-set-key [remap query-replace-regexp] 'vr/query-replace)
  (global-set-key [remap replace-regexp] 'vr/replace))


(user/search-replace-init)


(provide 'ux/search-replace)
;;; search-replace.el ends here
