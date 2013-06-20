(add-hook 'c-mode-common-hook
          (lambda ()
            (setq ac-sources '(ac-source-clang-async))
            (ac-clang-launch-completion-process)))
