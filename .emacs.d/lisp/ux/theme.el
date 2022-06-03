;;; theme.el --- Configure Emacs theme -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package custom
  :ensure nil
  :config
  (use-package solarized-dholm-theme
    :quelpa (solarized-dholm-theme
             :fetcher github
             :repo "dholm/solarized-theme")
    :ensure t
    :config
    (setq
     ;; Make the fringe stand out.
     solarized-distinct-fringe-background t)

    (load-theme 'solarized-dholm t)))

(use-package faces
  :ensure nil
  :defer
  :bind-wrap
  (((:key :emacs :describe-face) . describe-face)
   ((:key :emacs :describe-all-faces) . list-faces-display))
  :config
  (when (display-graphic-p)
    (cond
     ((eq system-type 'darwin)
      (set-face-attribute 'default nil :family "Menlo" :height 110 :weight 'normal))
     ((eq system-type 'windows-nt)
      (set-face-attribute 'default nil :family "Consolas" :height 100 :weight 'normal))
     ((eq system-type 'gnu/linux)
      (set-face-attribute 'default nil :foundry "bitstream" :family "Meslo LG S DZ"
                          :height 74 :weight 'normal))))

  (use-package textsize
    :commands textsize-mode
    :bind-wrap
    (((:key :emacs :text-scale-reset) . textsize-reset)
     ((:key :emacs :text-scale-increase) . textsize-increment)
     ((:key :emacs :text-scale-decrease)  . textsize-decrement))
    :init (textsize-mode)
    :config
    (validate-setq
     textsize-default-points 11)))

(use-package ns-auto-titlebar
  :if (eq system-type 'darwin)
  :config
  (ns-auto-titlebar-mode))

(use-package face-remap
  :ensure nil
  :bind-wrap
  (((:key :emacs :text-scale-reset) . (lambda () (interactive)
                                        (text-scale-set 0)))
   ((:key :emacs :text-scale-increase) . text-scale-increase)
   ((:key :emacs :text-scale-decrease)  . text-scale-decrease)))


(provide 'ux/theme)
;;; theme.el ends here
