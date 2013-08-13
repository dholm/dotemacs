;;; auto-complete.el --- initializes auto complete package
;;; Commentary:
;;; Code:

(defun user/auto-complete-init ()
  "Initialize auto-complete."
  (require 'auto-complete-config)

  ;; Load default configuration
  (ac-config-default)

  (setq-default
   ;; Limit the number of candidates
   ac-candidate-limit 200
   ;; Automatically start completion
   ac-auto-start 0.8
   ;; Use fuzzy matching
   ac-use-fuzzy 1.5
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
         '(ac-candidate-face ((t (:foreground ,orange :background ,solarized-hl))))
         '(ac-selection-face ((t (:foreground ,cyan-hc :background ,cyan-lc))))
         '(ac-candidate-mouse-face ((t (:foreground ,cyan-lc :background ,cyan-hc))))
         '(ac-completion-face ((t (:foreground ,solarized-emph :underline t))))
         '(ac-gtags-candidate-face ((t (:foreground ,blue :background ,solarized-hl))))
         '(ac-gtags-selection-face ((t (:foreground ,blue-hc :background ,blue-lc))))
         '(ac-yasnippet-candidate-face ((t (:foreground ,yellow :background ,solarized-hl))))
         '(ac-yasnippet-selection-face ((t (:foreground ,yellow-hc :background ,yellow-lc))))))))

  ;; Enable auto-complete globally
  (global-auto-complete-mode t)
  (after-load 'diminish
    (diminish 'auto-complete-mode))

  ;;; (Bindings) ;;;
  (ac-set-trigger-key "TAB"))

(require-package '(:name auto-complete :after (user/auto-complete-init)))


(provide 'utilities/auto-complete)
;;; auto-complete.el ends here
