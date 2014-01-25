;;; theme.el --- Configure Emacs theme
;;; Commentary:
;;; Code:

(defun user/solarized-init ()
  "Initialize Solarized theme."
  (load-theme 'solarized t))


(defun user/theme-init ()
  "Initialize Emacs theme."
  (when (display-graphic-p)
    (cond
         ((eq system-type 'darwin)
          (set-face-attribute 'default nil :family "Menlo" :height 110 :weight 'normal))
         ((eq system-type 'windows-nt)
          (set-face-attribute 'default nil :family "Consolas" :height 100 :weight 'normal))
         ((eq system-type 'gnu/linux)
          (set-face-attribute 'default nil :foundry "bitstream" :family "Meslo LG S DZ"
                                         :height 100 :weight 'normal))))

  ;; Enable blinking cursor
  (blink-cursor-mode)

  (require-package '(:name solarized-theme
                           :type github
                           :pkgname "dholm/solarized-theme"
                           :prepare (add-to-list 'custom-theme-load-path default-directory)
                           :after (user/solarized-init))))

(user/theme-init)


(provide 'ux/theme)
;;; theme.el ends here
