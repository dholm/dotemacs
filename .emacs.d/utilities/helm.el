;;; (Initialization) ;;;
(require-package (:name helm :after (dholm/helm-init)))
(require-package (:name helm-descbinds :after (dholm/helm-descbinds-init)))
(require-package (:name helm-etags-plus))
(require-package (:name helm-build-command))
(require-package (:name helm-ls-git))
(require-package (:name helm-c-yasnippet))


(setq helm-idle-delay 0.3
      helm-input-idle-delay 0.3)


(defun dholm/helm-init ()
  (helm-mode)
  (diminish 'helm-mode)
  ;; Filter out boring buffers
  (loop for exp in '("\\*clang-complete" "\\*CEDET global" "\\*tramp/scpc"
                     "\\*epc con" "\\*Pymacs" "\\*Completions\\*")
        do (add-to-list 'helm-boring-buffer-regexp-list exp))

  ;; Filter out boring files
  (loop for ext in '("\\.elc$" "\\.pyc$" "^#.+#$")
        do (add-to-list 'helm-boring-file-regexp-list ext))

  ;; Only override if helm is installed
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list))

(defun dholm/helm-descbinds-init ()
  (helm-descbinds-install))


;;; (Bindings) ;;;
(global-set-key (kbd "M-.") 'helm-etags+-select)
(global-set-key (kbd "M-*") 'helm-etags+-history)
(global-set-key (kbd "M-,") 'helm-etags+-history-action-go-back)
(global-set-key (kbd "M-/") 'helm-etags+-history-action-go-forward)


(provide 'utilities/helm)
