(require 'auto-complete-config)

(setq ac-auto-start nil
      ac-expand-on-auto-complete nil
      ac-quick-help-delay 0.5
      ;; Store the completion history in the cache directory
      ac-comphist-file (path-join *user-cache-directory* "ac-comphist.dat")
      ac-use-quick-help t)
(add-to-list 'ac-dictionary-directories (path-join *user-el-get-directory* "auto-complete" "ac-dict"))
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(ac-config-default)


;; Workaround for flyspell mode
(defun dholm/flymake-mode-hook ()
  ;; auto-complete workaround
  (ac-flyspell-workaround))
(add-hook 'flymake-mode-hook 'dholm/flymake-mode-hook)


;; Enable auto-complete globally
(global-auto-complete-mode t)
(diminish 'auto-complete-mode)
