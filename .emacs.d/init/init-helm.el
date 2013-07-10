;;; (Initialization) ;;;

(setq helm-idle-delay 0.3
      helm-input-idle-delay 0.3)

(helm-mode)

;; Filter out boring buffers
(loop for exp in '("\\*clang-complete" "\\*CEDET global" "\\*tramp/scpc"
                   "\\*epc con" "\\*Pymacs" "\\*Completions\\*")
      do (add-to-list 'helm-boring-buffer-regexp-list exp))

;; Filter out boring files
(loop for ext in '("\\.elc$" "\\.pyc$" "^#.+#$")
      do (add-to-list 'helm-boring-file-regexp-list ext))


;;; (Key Bindings) ;;;
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
