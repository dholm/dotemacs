;;; (Initialization) ;;;
(require-package (:name page-break-lines
                        :after (dholm/page-break-lines-init)))

(defun dholm/page-break-lines-init ()
  (require 'page-break-lines)
  (global-page-break-lines-mode)
  (diminish 'page-break-lines-mode))


(provide 'ux/page-break-lines)
