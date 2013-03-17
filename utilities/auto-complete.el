;; Visual popup user interface, required by auto-complete
(push "~/.emacs.d/utilities/popup-el" load-path)

(push "~/.emacs.d/utilities/auto-complete" load-path)

(require 'auto-complete-config)
(when (featurep 'auto-complete)
  (ac-config-default)
  (setq ac-auto-start nil)
  (setq ac-quick-help-delay 0.5)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/utilities/auto-complete/ac-dict")
  ;; Store the completion history in the cache directory
  (setq ac-comphist-file "~/.emacs.cache/ac-comphist.dat")

  (when (featurep 'cedet)
    ;; Use semantic as a source for auto complete
    (setq ac-sources '(ac-source-semantic)))

  (push "~/.emacs.d/utilities/auto-complete-clang" load-path)
  (require 'auto-complete-clang)
  (when (featurep 'auto-complete-clang)
    (add-hook 'c-mode-common-hook
              (lambda ()
                (setq ac-sources (append '(ac-source-clang) ac-sources)))))

  (add-hook 'auto-complete-mode-hook 'ac-common-setup)

  ;; Enable auto-complete globally
  (global-auto-complete-mode t))
