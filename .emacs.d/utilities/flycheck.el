;;; flycheck --- flycheck configuration
;;; Commentary:
;;; Code:

(defun dholm/flycheck-init ()
  "Initialize flycheck."
  (require 'flycheck)
  (global-flycheck-mode t)

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         ,@(if window-system
               `('(flycheck-error ((t (:foreground ,red :underline (:style wave :color ,red) :inherit unspecified))))
                 '(flycheck-warning ((t (:foreground ,yellow :underline (:style wave :color ,yellow) :inherit unspecified)))))
             `('(flycheck-error ((t (:foreground ,red :underline t))))
               '(flycheck-warning ((t (:foreground ,yellow :underline t))))))
         '(flycheck-fringe-error ((t (:foreground ,red :weight bold))))
         '(flycheck-fringe-warning ((t (:foreground ,yellow :weight bold)))))))))


(defun dholm/flycheck-color-mode-line-init ()
  "Initialize flycheck color mode."
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


(require-package '(:name flycheck
			 :after (dholm/flycheck-init)))

(require-package '(:name flycheck-color-mode-line
                         :features (flycheck-color-mode-line)
			 :type github
			 :pkgname "syl20bnr/flycheck-color-mode-line"
			 :depends (flycheck)
			 :after (dholm/flycheck-color-mode-line-init)))


(provide 'utilities/flycheck)
;;; flycheck.el ends here
