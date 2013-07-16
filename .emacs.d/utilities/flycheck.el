;;; (Initialization) ;;;

(defun dholm/flycheck-init ()
  (require 'flycheck)
  (global-flycheck-mode))

(defun dholm/flycheck-color-mode-line-init ()
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


(require-package (:name flycheck
                        :after (dholm/flycheck-init)))

(require-package (:name flycheck-color-mode-line
                          :type github
                          :pkgname "syl20bnr/flycheck-color-mode-line"
                          :depends (flycheck)
                          :after (dholm/flycheck-color-mode-line-init)))


(provide 'utilities/flycheck)
