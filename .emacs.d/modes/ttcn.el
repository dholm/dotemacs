;; Enables support for TTCN-3
(push "~/.emacs.d/modes/ttcn-el" load-path)
(require 'ttcn3)
(setq auto-mode-alist
      (cons '("\\.ttcn" . ttcn-3-mode) auto-mode-alist))


;; Set the TTCN-3 mode hook
(add-hook 'ttcn3-mode-hook
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
