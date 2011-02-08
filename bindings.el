;; (Key Bindings) ;;

;; Alias C-x C-m to M-x which is a bit awkward to reach
(global-set-key "\C-x\C-m" 'execute-extended-command)


;; Delete words with C-w and rebind kill region to C-x C-k
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)


;; Cycle through all buffers with Ctrl-Tab
(global-set-key (kbd "C-<tab>") 'stesla-rotate-buffers)
(global-set-key (kbd "C-M-<tab>") (lambda ()
  (interactive)
    (stesla-rotate-buffers -1)))


;; Show line numbers with F6
(global-set-key (kbd "<f6>") 'linum-mode)


;; Setup bindings for CEDET if it is available
(when (featurep 'cedet)
  (global-set-key (kbd "C-c .") 'semantic-ia-fast-jump)
  (global-set-key (kbd "C-c d") 'semantic-ia-show-doc)
  (global-set-key (kbd "C-c D") 'semantic-ia-describe-class)
  (global-set-key (kbd "C-c c") 'semantic-ia-complete-symbol)
  (global-set-key (kbd "C-c t") 'eassist-switch-h-cpp))
