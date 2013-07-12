;;; (Initialization) ;;;

(defun dholm/flycheck-init ()
  (require 'flycheck)
  (global-flycheck-mode)
  (diminish 'flycheck-mode))

(require-package (:name flycheck
                        :after (dholm/flycheck-init)))


(provide 'utilities/flycheck)
