;;; theme.el --- Configure Emacs theme -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--solarized-config ()
  "Initialize Solarized theme."
  (when window-system
    (setq-default
     ;; Force using the 256-color palette since it looks nicer.
     solarized-force-256color t
     ;; Make the fringe stand out.
     solarized-distinct-fringe-background t))

  (load-theme 'solarized t))

(use-package custom
  :ensure nil
  :config
  (require-package
   '(:name solarized-theme
           :type github
           :pkgname "dholm/solarized-theme"
           :prepare (add-to-list 'custom-theme-load-path default-directory)
           :after (user--solarized-config))))

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
                          :height 74 :weight 'normal)))))

(use-package face-remap
  :ensure nil
  :bind-wrap
  (((:key :emacs :text-scale-reset) . (lambda () (interactive)
                                        (text-scale-set 0)))
   ((:key :emacs :text-scale-increase) . text-scale-increase)
   ((:key :emacs :text-scale-decrease)  . text-scale-decrease)))


(provide 'ux/theme)
;;; theme.el ends here
