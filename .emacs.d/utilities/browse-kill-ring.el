;;; (Initialization) ;;;
(require-package (:name browse-kill-ring :after (dholm/browse-kill-ring-init)))

(defun dholm/browse-kill-ring-init ()
  (browse-kill-ring-default-keybindings))


(provide 'utilities/browse-kill-ring)
