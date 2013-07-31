;;; solarized --- solarized color theme
;;; Commentary:
;;; Code:

(defun dholm/solarized-init ()
  "Initialize Solarized theme."
  (load-theme 'solarized t)

  ;; faces for builtins
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized

       ;; basic coloring
       '(match ((t (:foreground ,solarized-emph :background ,solarized-hl :weight bold))))

       ;; compilation
       '(compilation-column-face ((t (:foreground ,cyan :underline nil))))
       '(compilation-column-number ((t (:inherit font-lock-doc-face :foreground ,cyan :underline nil))))
       '(compilation-enter-directory-face ((t (:foreground ,green :underline nil))))
       '(compilation-error ((t (:inherit error :underline nil))))
       '(compilation-error-face ((t (:foreground ,red : :underline nil))))
       '(compilation-face ((t (:foreground ,solarized-fg :underline nil))))
       '(compilation-info ((t (:foreground ,solarized-comment :underline nil :bold nil))))
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
       '(diff-added ((t (:foreground ,green :background ,solarized-bg))))
       '(diff-changed ((t (:foreground ,blue :background ,solarized-bg))))
       '(diff-removed ((t (:foreground ,red :background ,solarized-bg))))
       '(diff-header ((t (:background ,solarized-bg))))
       '(diff-file-header ((t (:foreground ,solarized-fg :background ,solarized-bg :weight bold))))
       '(diff-refine-added ((t :foreground ,solarized-bg :background ,green)))
       '(diff-refine-change ((t :foreground ,solarized-bg :background ,blue)))
       '(diff-refine-removed ((t (:foreground ,solarized-bg :background ,red))))

       ;; dired
       '(dired-directory ((t (:foreground ,blue :weight normal))))
       '(dired-flagged ((t (:foreground ,red))))
       '(dired-header ((t (:foreground ,solarized-bg :background ,blue))))
       '(dired-ignored ((t (:inherit shadow))))
       '(dired-mark ((t (:foreground ,yellow :weight bold))))
       '(dired-marked ((t (:foreground ,magenta :weight bold))))
       '(dired-perm-write ((t (:foreground ,solarized-fg :underline t))))
       '(dired-symlink ((t (:foreground ,cyan :slant italic))))
       '(dired-warning ((t (:foreground ,orange :underline t))))

       ;; dropdown
       '(dropdown-list-face ((t (:foreground ,cyan :background ,solarized-hl))))
       '(dropdown-list-selection-face ((t (:foreground ,cyan-hc :background ,cyan-lc))))

       ;; grep
       '(grep-context-face ((t (:foreground ,solarized-fg))))
       '(grep-error-face ((t (:foreground ,red :weight bold :underline t))))
       '(grep-hit-face ((t (:foreground ,blue))))
       '(grep-match-face ((t (:foreground ,orange :weight bold))))

       ;; ido-mode
       '(ido-first-match ((t (:foreground ,yellow :weight normal))))
       '(ido-only-match ((t (:foreground ,solarized-bg :background ,yellow :weight normal))))
       '(ido-subdir ((t (:foreground ,blue))))
       '(ido-incomplete-regexp ((t (:foreground ,red :weight bold))))
       '(ido-indicator ((t (:foreground ,solarized-bg :background ,red :width condensed))))
       '(ido-virtual ((t (:foreground ,cyan))))

       ;; man
       '(Man-overstrike ((t (:foreground ,blue :weight bold))))
       '(Man-reverse ((t (:foreground ,orange))))
       '(Man-underline ((t (:foreground ,green :underline t))))

       ;; whitespace-mode
       '(whitespace-space ((t (:foreground ,solarized-comment :background unspecified
                                           :inverse-video unspecified :slant italic))))
       `(whitespace-hspace ((t (:foreground ,solarized-emph :background unspecified
                                            :inverse-video unspecified))))
       `(whitespace-tab ((t (:foreground ,red :background unspecified
                                         :inverse-video unspecified :weight bold))))
       `(whitespace-newline ((t (:foreground ,solarized-comment :background unspecified
                                             :inverse-video unspecified))))
       `(whitespace-trailing ((t (:foreground ,orange-lc :background unspecified
                                              :inverse-video t))))
       `(whitespace-line ((t (:foreground ,magenta :background unspecified
                                          :inverse-video unspecified))))
       `(whitespace-space-before-tab ((t (:foreground unspecified :background ,red-lc
                                                      :inverse-video unspecified))))
       `(whitespace-indentation ((t (:foreground ,yellow :background unspecified
                                                 :inverse-video unspecified :weight bold))))
       `(whitespace-empty ((t (:foreground ,red-lc :background unspecified
                                           :inverse-video t))))
       `(whitespace-space-after-tab ((t (:foreground ,orange :background unspecified
                                                     :inverse-video t :weight bold))))))))

(require-package '(:name solarized-theme
			 :type github
			 :pkgname "dholm/solarized-theme"
			 :prepare (add-to-list 'custom-theme-load-path default-directory)
			 :after (dholm/solarized-init)))


(provide 'ux/solarized)
;;; solarized.el ends here
