;;; (Initialization) ;;;

(setq helm-idle-delay 0.1
      helm-input-idle-delay 0.1)

(helm-mode)

;; Filter out boring buffers
(loop for exp in '("\\*clang-complete" "\\*CEDET global")
      do (add-to-list 'helm-boring-buffer-regexp-list exp))

;; Filter out boring files
(loop for ext in '("\\.elc$" "\\.pyc$" "^#.+#$")
      do (add-to-list 'helm-boring-file-regexp-list ext))


;;; (Key Bindings) ;;;
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
