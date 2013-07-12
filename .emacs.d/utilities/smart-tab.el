;;; (Initialization) ;;;
(require-package (:name smart-tab :after (dholm/smart-tab-init)))

(defun dholm/smart-tab-init ()
  (global-smart-tab-mode)
  (diminish 'smart-tab-mode))


(provide 'utilities/smart-tab)
