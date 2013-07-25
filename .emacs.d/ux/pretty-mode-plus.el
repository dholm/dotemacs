;;; pretty-mode-plus --- pretty-print special sequences
;;; Commentary:
;;; Code:

(defun dholm/pretty-mode-plus-init ()
  (require 'pretty-mode-plus)

  ;;; (Faces) ;;;
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized
       '(pretty-mode-symbol-face  ((t (:foreground ,green)))))))

  (global-pretty-mode t))

(require-package '(:name pretty-mode-plus
			 :type elpa
			 :repo ("marmalade" . "http://marmalade-repo.org/packages/")
			 :after (dholm/pretty-mode-plus-init)))


(provide 'ux/pretty-mode-plus)
;;; pretty-mode-plus.el ends here
