;;; theme.el --- Configure Emacs theme
;;; Commentary:
;;; Code:

(defun user/theme-init ()
  "Initialize Emacs theme."
  ;; Make comments stand out.
  (set-face-foreground 'font-lock-comment-face "red")
  (set-face-foreground 'font-lock-comment-delimiter-face "red")

  ;; Font when in graphics mode.
  (when (display-graphic-p)
    (if (eq system-type 'windows-nt)
        (set-default-font "consolas-10")
      (set-default-font "Meslo LG S DZ:pixelsize=10:foundry=bitstream:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")))

  ;; Enable blinking cursor
  (blink-cursor-mode)

  (require-package '(:name solarized-theme
                           :type github
                           :pkgname "dholm/solarized-theme"
                           :prepare (add-to-list 'custom-theme-load-path default-directory)
                           :after (user/solarized-init))))


(defun user/solarized-init ()
  "Initialize Solarized theme."
  (load-theme 'solarized t))


(user/theme-init)


(provide 'ux/theme)
;;; theme.el ends here
