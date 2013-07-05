(require 'auto-complete-config)

(ac-config-default)
(setq ac-auto-start nil
      ac-quick-help-delay 0.5
      ;; Store the completion history in the cache directory
      ac-comphist-file "~/.emacs.cache/ac-comphist.dat")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/ac-dict")

(add-hook 'auto-complete-mode-hook 'ac-common-setup)

;; Enable auto-complete globally
(global-auto-complete-mode t)
