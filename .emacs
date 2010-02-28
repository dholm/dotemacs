;; (Coloring and Themes) ;;

(require 'color-theme-subdued)
(color-theme-subdued)


;; Highlighting "TODO", "FIXME" and friends
(add-hook 'c-mode-common-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 
        1 font-lock-warning-face t)))))


;; (Utilities) ;;

;; Show line numbers with F6
;; http://stud4.tuwien.ac.at/~e0225855/linum/linum.html
(require 'linum)
(global-set-key (kbd "<f6>") 'linum-mode)


;; Show trailing whitespace
(setq show-trailing-whitespace t)


;; Magit
(require 'magit)



;; (Code Conventions) ;;

(setq c-default-style "Linux")
(setq c++-default-style "Stroustrup")

(add-hook 'c-mode-common-hook
  (lambda()
    (require 'dtrt-indent)
    (dtrt-indent-mode t)))

(add-hook 'c++-mode-common-hook
  (lambda()
    (require 'dtrt-indent)
    (dtrt-indent-mode t)))

