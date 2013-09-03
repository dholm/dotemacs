;;; navigation.el --- Set up Emacs buffer navigation
;;; Commentary:
;;; Code:

(defun user/navigation-init ()
  "Set up Emacs buffer navigation."
  (require-package '(:name ace-jump-mode :after (user/ace-jump-mode-init)))
  (require-package '(:name smart-forward :after (user/smart-forward-init)))

  ;;; (Bindings) ;;;
  ;; Binds goto-line to navigation command g which is easier to access than M-g g
  (define-key user/navigation-map (kbd "g") 'goto-line))


(defun user/ace-jump-mode-init ()
  "Initialize ace jump mode."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(ace-jump-face-background ((t (:foreground ,solarized-comment :background ,solarized-bg
                                                     :inverse-video nil))))
         '(ace-jump-face-foreground ((t (:foreground ,red :background ,solarized-bg
                                                     :inverse-video nil :weight bold))))))))

  ;;; (Bindings) ;;;
  (define-key user/navigation-map (kbd "a") 'ace-jump-mode))


(defun user/smart-forward-init ()
  "Initialize smart-forward."
  (define-key user/navigation-map (kbd "s f") 'smart-forward)
  (define-key user/navigation-map (kbd "s b") 'smart-backward)
  (define-key user/navigation-map (kbd "s p") 'smart-up)
  (define-key user/navigation-map (kbd "s n") 'smart-down))


(user/navigation-init)


(provide 'ux/navigation)
;;; navigation.el ends here
