
(add-hook 'cperl-mode-hook
	  (lambda ()
	    (auto-complete-mode t)
            (set (make-local-variable 'ac-sources)
                 (append ac-sources '(ac-source-perl-completion)))))
