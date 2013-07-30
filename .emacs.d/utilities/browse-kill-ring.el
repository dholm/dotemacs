;;; browse-kill-ring --- initializes Browse Kill Ring
;;; Commentary:
;;; Code:

(defun dholm/browse-kill-ring-init ()
  "Initialize browse kill ring."
  (require 'browse-kill-ring)
  (define-key dholm/utilities-map (kbd "k") 'browse-kill-ring))

(require-package '(:name browse-kill-ring :after (dholm/browse-kill-ring-init)))


(provide 'utilities/browse-kill-ring)
;;; browse-kill-ring.el ends here
