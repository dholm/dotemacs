;;; auto-complete --- initializes auto complete package
;;; Commentary:
;;; Code:

(defun dholm/auto-complete-init ()
  "Initialize auto-complete."
  (require 'auto-complete-config)

  ;; Load default configuration
  (ac-config-default)

  (setq-default
   ;; Automatically start completion after 0.8s
   ac-auto-start nil
   ;; Use fuzzy matching
   ac-use-fuzzy t
   ;; Do not show menu unless requested
   ac-auto-show-menu nil
   ;; Allow normal navigation keys in menu
   ac-use-menu-map t
   ;; Do not auto-expand common candidates
   ac-expand-on-auto-complete nil
   ;; Show quick help popup after half a second
   ac-use-quick-help t
   ac-quick-help-delay 0.5
   ;; Use complete when auto-complete is unavailable
   tab-always-indent 'complete
   ;; Store the completion history in the cache directory
   ac-comphist-file (path-join *user-cache-directory* "ac-comphist.dat"))

  (add-to-list 'ac-dictionary-directories (path-join *user-el-get-directory* "auto-complete" "ac-dict"))
  (add-to-list 'completion-styles 'initials t)

  ;; Install workaround for flyspell
  (add-hook 'flymake-mode-hook 'ac-flyspell-workaround)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(ac-candidate-face ((t (:foreground ,orange :background ,base02))))
         '(ac-selection-face ((t (:foreground ,base03 :background ,cyan))))
         '(ac-candidate-mouse-face ((t (:foreground ,base03 :background ,cyan))))
         '(ac-completion-face ((t (:foreground ,base1 :underline t))))
         '(ac-gtags-candidate-face ((t (:foreground ,blue :background ,base02))))
         '(ac-gtags-selection-face ((t (:foreground ,base03 :background ,blue))))
         '(ac-yasnippet-candidate-face ((t (:foreground ,yellow :background ,base02))))
         '(ac-yasnippet-selection-face ((t (:foreground ,base03 :background ,yellow))))))))

  ;; Enable auto-complete globally
  (global-auto-complete-mode t)
  (after-load 'diminish
    (diminish 'auto-complete-mode)))

(require-package '(:name auto-complete :after (dholm/auto-complete-init)))
(require-package '(:name auto-complete-yasnippet :depends (yasnippet)))


(provide 'utilities/auto-complete)
;;; auto-complete.el ends here
