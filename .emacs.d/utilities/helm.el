;;; helm --- improved Emacs control
;;; Commentary:
;;; Code:

(defun dholm/helm-init ()
  "Initialize helm."
  ;; Enable helm mode
  (helm-mode t)
  (diminish 'helm-mode)

  (setq-default
   helm-idle-delay 0.2
   helm-input-idle-delay 0.2)

  ;; Filter out boring buffers
  (loop for exp in '("\\*clang-complete" "\\*CEDET global" "\\*tramp/scpc"
                     "\\*epc con" "\\*Pymacs" "\\*Completions\\*")
        do (add-to-list 'helm-boring-buffer-regexp-list exp))

  ;; Filter out boring files
  (loop for ext in '("\\.elc$" "\\.pyc$" "^#.+#$")
        do (add-to-list 'helm-boring-file-regexp-list ext))

  ;;; (Faces) ;;;
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized
       '(helm-apt-deinstalled ((t (:foreground ,base01))))
       '(helm-apt-installed ((t (:foreground ,green))))
       '(helm-bookmark-directory ((t (:inherit helm-ff-directory))))
       '(helm-bookmark-file ((t (:foreground ,base0))))
       '(helm-bookmark-gnus ((t (:foreground ,cyan))))
       '(helm-bookmark-info ((t (:foreground ,green))))
       '(helm-bookmark-man ((t (:foreground ,violet))))
       '(helm-bookmark-w3m ((t (:foreground ,yellow))))
       '(helm-bookmarks-su ((t (:foreground ,orange))))
       '(helm-buffer-not-saved ((t (:foreground ,orange))))
       '(helm-buffer-saved-out ((t (:foreground ,red :background ,base03 ,@fmt-revr))))
       '(helm-buffer-size ((t (:foreground ,base01))))
       '(helm-candidate-number ((t (:background ,base02 :foreground ,base1 ,@fmt-bold))))
       '(helm-ff-directory ((t (:background ,base03  :foreground ,blue))))
       '(helm-ff-executable ((t (:foreground ,green))))
       '(helm-ff-file ((t (:background ,base03 :foreground ,base0))))
       '(helm-ff-invalid-symlink ((t (:background ,base03 :foreground ,orange ,@fmt-ital))))
       '(helm-ff-prefix ((t (:background ,yellow :foreground ,base03))))
       '(helm-ff-symlink ((t (:foreground ,cyan))))
       '(helm-grep-file ((t (:foreground ,cyan ,@fmt-undr))))
       '(helm-grep-finish ((t (:foreground ,green))))
       '(helm-grep-lineno ((t (:foreground ,orange))))
       '(helm-grep-match ((t (:inherit match))))
       '(helm-grep-running ((t (:foreground ,red))))
       '(helm-header ((t (:inherit header-line))))
       '(helm-lisp-completion-info ((t (:foreground ,base0))))
       '(helm-lisp-show-completion ((t (:foreground ,yellow :background ,base02 ,@fmt-bold))))
       '(helm-M-x-key ((t (:foreground ,orange ,@fmt-undr))))
       '(helm-moccur-buffer ((t (:foreground ,cyan ,@fmt-undr))))
       '(helm-match ((t (:inherit match))))
       '(helm-selection ((t (:background ,base02 ,@fmt-undr))))
       '(helm-selection-line ((t (:background ,base02 :foreground ,base1 ,@fmt-none))))
       '(helm-separator ((t (:foreground ,red))))
       '(helm-source-header ((t (:background ,blue :foreground ,base03 ,@fmt-none))))
       '(helm-time-zone-current ((t (:foreground ,green))))
       '(helm-time-zone-home ((t (:foreground ,red))))
       '(helm-visible-mark ((t (:background ,base03 :foreground ,magenta ,@fmt-bold)))))))

  ;;; (Bindings) ;;;
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "M-.") 'helm-etags+-select)
  (global-set-key (kbd "M-*") 'helm-etags+-history)
  (global-set-key (kbd "M-,") 'helm-etags+-history-action-go-back)
  (global-set-key (kbd "M-/") 'helm-etags+-history-action-go-forward))


(defun dholm/helm-descbinds-init ()
  "Initialize helm-descbinds."
  (require 'helm-descbinds)
  (helm-descbinds-mode t))


(require-package '(:name helm :after (dholm/helm-init)))
(require-package '(:name helm-descbinds :after (dholm/helm-descbinds-init)))
(require-package '(:name helm-etags-plus))
(require-package '(:name helm-build-command))
(require-package '(:name helm-ls-git
			 :type github
			 :pkgname "emacs-helm/helm-ls-git"))
(require-package '(:name helm-c-yasnippet
			 :type github
			 :pkgname "emacs-helm/helm-c-yasnippet"
			 :features helm-c-yasnippet
			 :depends (helm yasnippet)))


(provide 'utilities/helm)
;;; helm.el ends here
