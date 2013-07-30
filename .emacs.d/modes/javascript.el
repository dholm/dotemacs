;;; javascript --- initializes JavaScript modes
;;; Commentary:
;;; Code:

(defun dholm/js2-mode-hook ()
  "JavaScript mode hook."
  ;; Enable js2-mode
  (js2-mode t)
  ;; Load CEDET
  (dholm/javascript-mode-cedet-hook)
  ;; Configure js2-mode
  (setq js2-use-font-lock-faces t
        js2-mode-must-byte-compile nil
        js2-indent-on-enter-key t
        js2-auto-indent-p t
        js2-bounce-indent-p nil
        js2-basic-offset 2)
  ;; Configure autocompletion
  (set (make-local-variable 'ac-auto-start) 3)
  (set (make-local-variable 'ac-auto-show-menu) t))

(defun dholm/javascript-mode-cedet-hook ()
  "JavaScript CEDET support hook."
  (dholm/cedet-hook)
  (require 'semantic/wisent/javascript))


(defun dholm/js2-mode-init ()
  "Initialize js2 mode."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(js2-error ((t (:foreground ,red))))
         '(js2-external-variable ((t (:foreground ,orange))))
         '(js2-function-param ((t (:foreground ,green))))
         '(js2-instance-member ((t (:foreground ,magenta))))
         '(js2-jsdoc-html-tag-delimiter ((t (:foreground ,cyan))))
         '(js2-jsdoc-html-tag-name ((t (:foreground ,orange))))
         '(js2-jsdoc-tag ((t (:foreground ,cyan))))
         '(js2-jsdoc-type ((t (:foreground ,blue))))
         '(js2-jsdoc-value ((t (:foreground ,violet))))
         '(js2-magic-paren ((t (:underline t))))
         '(js2-private-function-call ((t (:foreground ,yellow))))
         '(js2-private-member ((t (:foreground ,blue))))
         '(js2-warning ((t (:underline ,orange))))))))

  (add-hook 'js2-mode-hook 'dholm/js2-mode-hook)
  (add-auto-mode 'js2-mode "\\.js$"))

(require-package '(:name js2-mode :after (dholm/js2-mode-init)))


(provide 'modes/javascript)
;;; javascript.el ends here
