
(add-hook  'cperl-mode-hook
           (lambda ()
             (auto-complete-mode t)
             (make-variable-buffer-local 'ac-sources)
             (setq ac-sources
                   '(ac-source-perl-completion))))
