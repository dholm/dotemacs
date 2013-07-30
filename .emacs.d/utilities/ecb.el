;;; ecb --- Emacs Code Browser
;;; Commentary:
;;; Code:

(defun dholm/ecb-init ()
  "Initialize Emacs code browser."
  (require 'ecb-autoloads)

  ;; (ECB Layout) ;;
  (setq-default
   ecb-layout-name "left7"
   ecb-layout-window-sizes '(("left7"
			      (ecb-directories-buffer-name 0.17 . 0.6428571428571429)
			      (ecb-sources-buffer-name 0.17 . 0.3392857142857143)
			      (ecb-methods-buffer-name 0.25 . 0.6428571428571429)
			      (ecb-history-buffer-name 0.25 . 0.3392857142857143)))
   ecb-show-sources-in-directories-buffer 'always
   ecb-compile-window-height 12)

  ;;; (Bindings) ;;;
  (define-key dholm/utilities-map (kbd "e") 'ecb-activate)

  ;;; quick navigation between ecb windows
  (define-key dholm/navigation-map (kbd "1") 'ecb-goto-window-edit1)
  (define-key dholm/navigation-map (kbd "2") 'ecb-goto-window-directories)
  (define-key dholm/navigation-map (kbd "3") 'ecb-goto-window-history)
  (define-key dholm/navigation-map (kbd "4") 'ecb-goto-window-methods)
  (define-key dholm/navigation-map (kbd "5") 'ecb-goto-window-compilation)

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(ecb-default-highlight-face ((t (:foreground ,base03 :background ,blue))))
         '(ecb-history-bucket-node-dir-soure-path-face ((t (:inherit ecb-history-bucket-node-face :foreground ,yellow))))
         '(ecb-source-in-directories-buffer-face ((t (:inherit ecb-directories-general-face :foreground ,base0))))
         '(ecb-history-dead-buffer-face ((t (:inherit ecb-history-general-face :foreground ,base01))))
         '(ecb-directory-not-accessible-face ((t (:inherit ecb-directories-general-face :foreground ,base01))))
         '(ecb-bucket-node-face ((t (:inherit ecb-default-general-face :foreground ,blue))))
         '(ecb-tag-header-face ((t (:background ,base02))))
         '(ecb-analyse-bucket-element-face ((t (:inherit ecb-analyse-general-face :foreground ,green))))
         '(ecb-directories-general-face ((t (:inherit ecb-default-general-face :height 1.0))))
         '(ecb-method-non-semantic-face ((t (:inherit ecb-methods-general-face :foreground ,cyan))))
         '(ecb-mode-line-prefix-face ((t (:foreground ,green))))
         '(ecb-tree-guide-line-face ((t (:inherit ecb-default-general-face :foreground ,base02 :height 1.0)))))))))

(require-package '(:name ecb :after (dholm/ecb-init)))


(provide 'utilities/ecb)
;;; ecb.el ends here
