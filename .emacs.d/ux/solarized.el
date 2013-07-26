;;; solarized --- solarized color theme
;;; Commentary:
;;; Code:

(require-package '(:name solarized-theme
			 :type github
			 :pkgname "awmckinley/solarized-theme"
			 :prepare (add-to-list 'custom-theme-load-path default-directory)
			 :after (dholm/solarized-init)))

(defun dholm/solarized-init ()
  "Initialize Solarized theme."
  (load-theme 'solarized t)

  ;; faces for builtins
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized

       ;; basic coloring
       '(match ((t (:foreground ,base1 :background ,base02 :weight bold))))

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
       '(diff-added ((t (:foreground ,green ,@back))))
       '(diff-changed ((t (:foreground ,blue ,@back))))
       '(diff-removed ((t (:foreground ,red ,@back))))
       '(diff-header ((t (,@back))))
       '(diff-file-header ((t (:foreground ,base0 ,@back :weight bold))))
       '(diff-refine-added ((t :foreground ,base03 :background ,green)))
       '(diff-refine-change ((t :foreground ,base03 :background ,blue)))
       '(diff-refine-removed ((t (:foreground ,base03 :background ,red))))

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
       '(Man-underline ((t (:foreground ,green :underline t))))

       ;; whitespace-mode
       '(whitespace-space ((t (:background unspecified :foreground ,base01 :inverse-video unspecified :slant italic))))
       `(whitespace-hspace ((t (:background unspecified :foreground ,base1 :inverse-video unspecified))))
       `(whitespace-tab ((t (:background unspecified :foreground ,red :inverse-video unspecified :weight bold))))
       `(whitespace-newline ((t (:background unspecified :foreground ,base01 :inverse-video unspecified))))
       `(whitespace-trailing ((t (:background unspecified :foreground ,orange :inverse-video t))))
       `(whitespace-line ((t (:background unspecified :foreground ,magenta :inverse-video unspecified))))
       `(whitespace-space-before-tab ((t (:background ,red :foreground unspecified :inverse-video unspecified))))
       `(whitespace-indentation ((t (:background unspecified :foreground ,yellow :inverse-video unspecified :weight bold))))
       `(whitespace-empty ((t (:background unspecified :foreground ,red :inverse-video t))))
       `(whitespace-space-after-tab ((t (:background unspecified :foreground ,orange :inverse-video t :weight bold))))))))


(provide 'ux/solarized)
;;; solarized.el ends here
