;;; pretty-mode-plus --- pretty-print special sequences
;;; Commentary:
;;; Code:

(defun dholm/pretty-mode-plus-init ()
  "Initialize pretty mode plus."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(pretty-mode-symbol-face  ((t (:foreground ,green))))))))
  ;; Enable pretty mode plus globally
  (global-pretty-mode t))

(require-package '(:name pretty-mode-plus
			 :type github
			 :pkgname "akatov/pretty-mode-plus"
                         :prepare (progn
                                    (autoload 'turn-on-pretty-mode "pretty-mode-plus")
                                    (autoload 'global-pretty-mode "pretty-mode-plus"))
			 :after (dholm/pretty-mode-plus-init)))


(provide 'ux/pretty-mode-plus)
;;; pretty-mode-plus.el ends here
