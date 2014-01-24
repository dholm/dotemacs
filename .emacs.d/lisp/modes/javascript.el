;;; javascript.el --- initializes JavaScript modes
;;; Commentary:
;;; Code:

(defun user/javascript-mode-common-hook ()
  "JavaScript common mode hook."
  ;; Load CEDET
  (user/javascript-mode-cedet-hook)

  (tern-mode t))


(defun user/javascript-mode-hook ()
  "JavaScript mode hook."
  (user/javascript-mode-common-hook))


(defun user/js3-mode-hook ()
  "JS3 mode hook."
  (user/javascript-mode-common-hook)
  ;; Enable smart indentation
  (smart-tabs-mode t)
  ;; Enable Flycheck
  (flycheck-mode t))


(defun user/javascript-mode-cedet-hook ()
  "JavaScript CEDET support hook."
  (when (featurep 'cedet)
    (user/cedet-hook)
    (require 'semantic/wisent/javascript)))


(defun user/js3-mode-init ()
  "Initialize js3 mode."
  (setq-default
   ;; Configure indentation
   js3-indent-on-enter-key t
   js3-auto-indent-p t
   ;; Idle timeout before reparsing buffer
   js3-idle-timer-delay 0.5
   ;; Do not load browser-specific functions
   js3-include-browser-externs nil
   ;; Support Node.js
   js3-skip-preprocessor-directives t
   ;; Disable error parsing in favor of Flycheck
   js3-strict-missing-semi-warning nil)

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(js3-warning-face ((t (:underline ,orange))))
         '(js3-error-face ((t (:foreground ,red))))
         '(js3-external-variable-face ((t (:foreground ,orange))))
         '(js3-function-param-face ((t (:foreground ,green))))
         '(js3-instance-member-face ((t (:foreground ,magenta))))
         '(js3-jsdoc-html-tag-delimiter-face ((t (:foreground ,cyan))))
         '(js3-jsdoc-html-tag-name-face ((t (:foreground ,orange))))
         '(js3-jsdoc-tag-face ((t (:foreground ,cyan))))
         '(js3-jsdoc-type-face ((t (:foreground ,blue))))
         '(js3-jsdoc-value-face ((t (:foreground ,violet))))
         '(js3-magic-paren-face ((t (:underline t))))
         '(js3-private-function-call-face ((t (:foreground ,yellow))))
         '(js3-private-member-face ((t (:foreground ,blue))))))))

  (add-hook 'js3-mode-hook 'user/js3-mode-hook)
  (add-auto-mode 'js3-mode "\\.js$")
  (add-magic-mode 'js3-mode "#!/usr/bin/env node"))


(defun user/javascript-mode-init ()
  "Initialize JavaScript mode."
  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.js"))

  (add-hook 'javascript-mode-hook 'user/javascript-mode-hook)
  (add-auto-mode 'javascript-mode "\\.json$")

  (require-package '(:name js3-mode :after (user/js3-mode-init))))

(user/javascript-mode-init)


(provide 'modes/javascript)
;;; javascript.el ends here
