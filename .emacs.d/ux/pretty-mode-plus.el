;;; (Initialization) ;;;
(require-package (:name pretty-mode-plus
                        :after (dholm/pretty-mode-plus-init)))

(defun dholm/pretty-mode-plus-init ()
  (require 'pretty-mode-plus)
  (global-pretty-mode))


(provide 'ux/pretty-mode-plus)
