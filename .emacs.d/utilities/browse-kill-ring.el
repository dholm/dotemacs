;;; browse-kill-ring --- initializes Browse Kill Ring
;;; Commentary:
;;; Code:

(require-package '(:name browse-kill-ring :after (dholm/browse-kill-ring-init)))

(defun dholm/browse-kill-ring-init ()
  (require 'browse-kill-ring)
  (browse-kill-ring-default-keybindings))


(provide 'utilities/browse-kill-ring)
;;; browse-kill-ring.el ends here
