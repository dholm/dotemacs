;; (Key Bindings) ;;

;; Function keys
(global-set-key [f3] 'shell)
(global-set-key [f7] 'compile)
(global-set-key [f8] 'gdb)


;; Alias C-x C-m to M-x which is a bit awkward to reach
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)


;; Delete words with C-w and rebind kill region to C-x C-k
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)


;; Show line numbers with F6
(global-set-key (kbd "<f6>") 'linum-mode)


;; Setup bindings for CEDET if it is available
(when (featurep 'cedet)
  (global-set-key (kbd "C-c .") 'semantic-ia-fast-jump)
  (global-set-key (kbd "C-c d") 'semantic-ia-show-doc)
  (global-set-key (kbd "C-c D") 'semantic-ia-describe-class)
  (global-set-key (kbd "C-c c") 'semantic-ia-complete-symbol)
  (global-set-key (kbd "C-c t") 'eassist-switch-h-cpp))


;; Spell check the current word
(global-set-key "\C-c\C-w" 'ispell-word)


;; Use regex searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)