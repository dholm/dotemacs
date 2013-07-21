(require-package '(:name auto-complete :after (dholm/auto-complete-init)))

(require 'utilities/yasnippet)
(require-package '(:name auto-complete-yasnippet))


;; Workaround for flyspell mode
(defun dholm/flymake-mode-hook ()
  ;; auto-complete workaround
  (ac-flyspell-workaround))


(defun dholm/auto-complete-init ()
  (require 'auto-complete-config)

  (ac-config-default)

  (add-to-list 'ac-dictionary-directories (path-join *user-el-get-directory* "auto-complete" "ac-dict"))
  (add-to-list 'ac-sources 'ac-source-yasnippet)

  (add-hook 'flymake-mode-hook 'dholm/flymake-mode-hook)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)

  ;; Enable auto-complete globally
  (global-auto-complete-mode t)
  (diminish 'auto-complete-mode))


(setq ac-auto-start nil
      ac-expand-on-auto-complete nil
      ac-quick-help-delay 0.5
      ;; Store the completion history in the cache directory
      ac-comphist-file (path-join *user-cache-directory* "ac-comphist.dat")
      ac-use-quick-help t)


(provide 'utilities/auto-complete)
