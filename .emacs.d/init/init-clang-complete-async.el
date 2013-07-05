(add-hook 'c-mode-common-hook
          (lambda ()
            (set (make-local-variable 'ac-sources)
                 (append ac-sources '(ac-source-clang-async)))
            (ac-clang-launch-completion-process)))
