;;; search-replace.el --- Configuration for searching and replacing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
    :defer
    :bind* (([remap query-replace-regexp] . vr/query-replace)
            ([remap replace-regexp] . vr/replace)))

  (use-package replace-with-inflections
    :bind (:map search-map
           ("n" . query-replace-names-with-inflections)))

  (use-package anzu
    :defer
    :diminish anzu-mode
    :config
    (global-anzu-mode t))

  (use-package grep
    :defer
    :config
    (validate-setq
     ;; Highlight matches when using grep.
     grep-highlight-matches t)

    (when (eq system-type 'darwin)
      (when-let (gnu-find (executable-find "gfind"))
        (validate-setq find-program gnu-find)))
    (when-let (gnu-xargs (executable-find "gxargs"))
      (validate-setq xargs-program gnu-xargs))

    (use-package wgrep
      :defer
      :config
      (use-package wgrep-helm)
      (use-package wgrep-ag
        :if (executable-find "ag")))))

(user--search-replace-config)


(provide 'ux/search-replace)
;;; search-replace.el ends here
