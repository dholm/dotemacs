;;; solarized --- solarized color theme
;;; Commentary:
;;; Code:

(require-package '(:name solarized-theme
			 :type github
			 :pkgname "awmckinley/solarized-theme"
			 :prepare (add-to-list 'custom-theme-load-path default-directory)
			 :after (dholm/solarized-init)))

(defun dholm/solarized-init ()
  (load-theme 'solarized t)

  ;; faces for builtins
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized

       ;; compilation
       '(compilation-column-face ((t (:foreground ,cyan :underline nil))))
       '(compilation-column-number ((t (:inherit font-lock-doc-face :foreground ,cyan :underline nil))))
       '(compilation-enter-directory-face ((t (:foreground ,green :underline nil))))
       '(compilation-error ((t (:inherit error :underline nil))))
       '(compilation-error-face ((t (:foreground ,red : :underline nil))))
       '(compilation-face ((t (:foreground ,base0 :underline nil))))
       '(compilation-info ((t (:foreground ,base01 :underline nil :bold nil))))
       '(compilation-info-face ((t (:foreground ,blue :underline nil))))
       '(compilation-leave-directory-face ((t (:foreground ,green :underline nil))))
       '(compilation-line-face ((t (:foreground ,green :underline nil))))
       '(compilation-line-number ((t (:foreground ,green :underline nil))))
       '(compilation-warning ((t (:inherit warning :underline nil))))
       '(compilation-warning-face ((t (:foreground ,yellow :weight normal :underline nil))))
       '(compilation-mode-line-exit ((t (:inherit compilation-info :foreground ,green :weight bold))))
       '(compilation-mode-line-fail ((t (:inherit compilation-error :foreground ,red :weight bold))))
       '(compilation-mode-line-run ((t (:foreground ,orange :weight bold))))

       ;; diff
       ,@(if window-system
	     `('(diff-added ((t (:foreground ,green ,@fmt-bold))))
	       '(diff-changed ((t (:foreground ,yellow ,@fmt-bold))))
	       '(diff-removed ((t (:foreground ,red ,@fmt-bold))))
	       '(diff-refine-change ((t (:foreground ,blue ,@back ,@fmt-bold)))))
	   `('(diff-added ((t (:foreground ,green))))
	     '(diff-changed ((t (:foreground ,yellow))))
	     '(diff-removed ((t (:foreground ,red))))
	     '(diff-refine-change ((t (:foreground ,blue ,@back))))))

       ;; dired
       '(dired-directory ((t (:foreground ,blue :weight normal))))
       '(dired-flagged ((t (:foreground ,red))))
       '(dired-header ((t (:foreground ,base03 :background ,blue))))
       '(dired-ignored ((t (:inherit shadow))))
       '(dired-mark ((t (:foreground ,yellow :weight bold))))
       '(dired-marked ((t (:foreground ,magenta :weight bold))))
       '(dired-perm-write ((t (:foreground ,base0 :underline t))))
       '(dired-symlink ((t (:foreground ,cyan :slant italic))))
       '(dired-warning ((t (:foreground ,orange :underline t))))

       ;; dropdown
       '(dropdown-list-face ((t (:background ,base02 :foreground ,cyan))))
       '(dropdown-list-selection-face ((t (:background ,base02 :foreground ,cyan))))

       ;; grep
       '(grep-context-face ((t (:foreground ,base0))))
       '(grep-error-face ((t (:foreground ,red :weight bold :underline t))))
       '(grep-hit-face ((t (:foreground ,blue))))
       '(grep-match-face ((t (:foreground ,orange :weight bold))))

       ;; ido-mode
       '(ido-first-match ((t (:foreground ,yellow :weight normal))))
       '(ido-only-match ((t (:foreground ,base03 :background ,yellow :weight normal))))
       '(ido-subdir ((t (:foreground ,blue))))
       '(ido-incomplete-regexp ((t (:foreground ,red :weight bold))))
       '(ido-indicator ((t (:background ,red :foreground ,base03 :width condensed))))
       '(ido-virtual ((t (:foreground ,cyan))))

       ;; man
       '(Man-overstrike ((t (:foreground ,blue :weight bold))))
       '(Man-reverse ((t (:foreground ,orange))))
       '(Man-underline ((t (:foreground ,green :underline t))))))))


(provide 'ux/solarized)
;;; solarized.el ends here
