
(require 'auto-complete-config)
(when (featurep 'auto-complete)
  (ac-config-default)
  (setq ac-auto-start nil)
  (setq ac-quick-help-delay 0.5)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/ac-dict")
  ;; Store the completion history in the cache directory
  (setq ac-comphist-file "~/.emacs.cache/ac-comphist.dat")

  (when (featurep 'cedet)
    ;; Use semantic as a source for auto complete
    (setq ac-sources '(ac-source-semantic)))

  (add-hook 'auto-complete-mode-hook 'ac-common-setup)

  ;; Enable auto-complete globally
  (global-auto-complete-mode t))
