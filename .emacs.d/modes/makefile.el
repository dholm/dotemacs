;; Sets some decent defaults for makefile-mode
(add-hook 'makefile-mode-hook
          (lambda ()
            ;; Use tabs for indent
            (setq indent-tabs-mode t)
            ;; Run spell-checker on strings and comments
            (flyspell-prog-mode)
            ;; Separate camel-case into separate words
            (subword-mode t)
            ;; Show trailing whitespace
            (setq show-trailing-whitespace t)
            (add-hook 'before-save-hook
                      ;; Delete trailing whitespace on save
                      'delete-trailing-whitespace nil t)))
