;; Load the PHP major mode
(require 'php-mode)

;; Set up helpers for php-mode
(add-hook 'php-mode-hook
          (lambda ()
            ;; Run spell-checker on strings and comments
            (flyspell-prog-mode)
            ;; Separate camel-case into separate words
            (subword-mode t)
            ;; Show trailing whitespace
            (setq show-trailing-whitespace t)
            (add-hook 'before-save-hook
                      ;; Delete trailing whitespace on save
                      'delete-trailing-whitespace nil t)
            (when (featurep 'dtrt-indent)
              ;; Enable dtrt-indent to attempt to identify the indentation rules used
              (dtrt-indent-mode t))))
