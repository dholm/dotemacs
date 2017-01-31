;;; search-replace.el --- Configuration for searching and replacing
;;; Commentary:
;;; Code:

(defun user--anzu-config ()
  "Initialize anzu."
  (global-anzu-mode t)
  (after-load 'diminish
    (diminish 'anzu-mode)))


(defun user--search-replace-config ()
  "Initialize Emacs search and replace."
  (validate-setq
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
  (use-package visual-regexp
    :ensure t
    :bind* (([remap query-replace-regexp] . vr/query-replace)
            ([remap replace-regexp] . vr/replace)))
  (use-package anzu
    :ensure t
    :config (user--anzu-config))
  (use-package grep
    :ensure t
    :config
    (validate-setq
     ;; Highlight matches when using grep.
     grep-highlight-matches t)))

(user--search-replace-config)


(provide 'ux/search-replace)
;;; search-replace.el ends here
