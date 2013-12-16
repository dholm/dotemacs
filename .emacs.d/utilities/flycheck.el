;;; flycheck.el --- flycheck configuration
;;; Commentary:
;;; Code:

(defun user/flycheck-mode-hook ()
  "Flycheck mode hook."
  ;;; (Bindings) ;;;
  (define-key user/code-map (kbd "E") 'flycheck-list-errors))


(defun user/flycheck-color-mode-line-init ()
  "Initialize flycheck color mode."
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


(defun user/flycheck-init ()
  "Initialize flycheck."
  (require 'flycheck)

  (setq-default
   ;; Wait five seconds before starting checker
   flycheck-idle-change-delay 5.0)
  (global-flycheck-mode t)

  (add-hook 'flycheck-mode-hook 'user/flycheck-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name flycheck-color-mode-line
                           :features (flycheck-color-mode-line)
                           :type github
                           :pkgname "syl20bnr/flycheck-color-mode-line"
                           :depends (flycheck)
                           :after (user/flycheck-color-mode-line-init)))

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(flycheck-error
           ((,'((supports :underline (:style wave)))
             (:underline (:style wave :color ,red) :inherit unspecified
                         :foreground ,red-hc :background ,red-lc))
            (t (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
         '(flycheck-warning
           ((,'((supports :underline (:style wave)))
             (:underline (:style wave :color ,yellow) :inherit unspecified
                         :foreground ,yellow-hc :background ,yellow-lc))
            (t (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))
         '(flycheck-fringe-error ((t (:foreground ,red-hc :background ,red-lc :weight bold))))
         '(flycheck-fringe-warning ((t (:foreground ,yellow-hc :background ,yellow-lc :weight bold)))))))))

(require-package '(:name flycheck
			 :after (user/flycheck-init)))


(provide 'utilities/flycheck)
;;; flycheck.el ends here
