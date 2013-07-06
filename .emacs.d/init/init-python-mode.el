(setq py-shell-name "ipython"
      py-load-pymacs-p t)

(add-hook 'python-mode-hook
          (lambda ()
            ;; Bind electric backspace to del which translates to backspace in
            ;; terminals.
            (define-key python-mode-map (kbd "DEL") 'py-electric-backspace)))
