;;; helm --- improved Emacs control
;;; Commentary:
;;; Code:

(defun user/helm-init ()
  "Initialize helm."
  ;; Enable helm mode
  (helm-mode t)
  (after-load 'diminish
    (diminish 'helm-mode))

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
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(helm-apt-deinstalled ((t (:foreground ,solarized-comment))))
         '(helm-apt-installed ((t (:foreground ,green))))
         '(helm-bookmark-directory ((t (:inherit helm-ff-directory))))
         '(helm-bookmark-file ((t (:foreground ,solarized-fg))))
         '(helm-bookmark-gnus ((t (:foreground ,cyan))))
         '(helm-bookmark-info ((t (:foreground ,green))))
         '(helm-bookmark-man ((t (:foreground ,violet))))
         '(helm-bookmark-w3m ((t (:foreground ,yellow))))
         '(helm-bookmarks-su ((t (:foreground ,orange))))
         '(helm-buffer-not-saved ((t (:foreground ,orange))))
         '(helm-buffer-saved-out ((t (:foreground ,red :background ,solarized-bg
                                                  :inverse-video t))))
         '(helm-buffer-size ((t (:foreground ,solarized-comment))))
         '(helm-candidate-number ((t (:foreground ,solarized-emph :background ,solarized-hl
                                                  :bold t))))
         '(helm-ff-directory ((t (:foreground ,blue :background ,solarized-bg))))
         '(helm-ff-executable ((t (:foreground ,green))))
         '(helm-ff-file ((t (:foreground ,solarized-fg :background ,solarized-bg))))
         '(helm-ff-invalid-symlink ((t (:foreground ,orange :background ,solarized-bg
                                                    :slant italic))))
         '(helm-ff-prefix ((t (:foreground ,solarized-bg :background ,yellow))))
         '(helm-ff-symlink ((t (:foreground ,cyan))))
         '(helm-grep-file ((t (:foreground ,cyan :underline t))))
         '(helm-grep-finish ((t (:foreground ,green))))
         '(helm-grep-lineno ((t (:foreground ,orange))))
         '(helm-grep-match ((t (:inherit match))))
         '(helm-grep-running ((t (:foreground ,red))))
         '(helm-header ((t (:inherit header-line))))
         '(helm-lisp-completion-info ((t (:foreground ,solarized-fg))))
         '(helm-lisp-show-completion ((t (:foreground ,yellow :background ,solarized-hl
                                                      :bold t))))
         '(helm-M-x-key ((t (:foreground ,orange :underline t))))
         '(helm-moccur-buffer ((t (:foreground ,cyan :underline t))))
         '(helm-match ((t (:inherit match))))
         '(helm-selection ((t (:background ,solarized-hl ,@fmt-undr))))
         '(helm-selection-line ((t (:foreground ,solarized-emph :background ,solarized-hl
                                                :underline nil))))
         '(helm-separator ((t (:foreground ,red))))
         '(helm-source-header ((t (:foreground ,solarized-bg :background ,blue-lc
                                               :underline nil))))
         '(helm-time-zone-current ((t (:foreground ,green))))
         '(helm-time-zone-home ((t (:foreground ,red))))
         '(helm-visible-mark ((t (:foreground ,magenta :background ,solarized-bg
                                              :bold t))))))))

  ;;; (Bindings) ;;;
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "M-.") 'helm-etags+-select)
  (global-set-key (kbd "M-*") 'helm-etags+-history)
  (global-set-key (kbd "M-,") 'helm-etags+-history-action-go-back)
  (global-set-key (kbd "M-/") 'helm-etags+-history-action-go-forward))


(defun user/helm-descbinds-init ()
  "Initialize helm-descbinds."
  (require 'helm-descbinds)
  (helm-descbinds-mode t))


(require-package '(:name helm :after (user/helm-init)))
(require-package '(:name helm-descbinds :after (user/helm-descbinds-init)))
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
