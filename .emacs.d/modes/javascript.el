;;; javascript.el --- initializes JavaScript modes
;;; Commentary:
;;; Code:

(defun user/js-mode-common-hook ()
  "JavaScript mode hook."
  ;; Load CEDET
  (user/javascript-mode-cedet-hook)
  ;; Enable tern if installed
  (when (el-get-package-is-installed 'tern)
    (tern-mode t)
    (define-key user/navigation-map (kbd "j") 'tern-find-definition)
    (define-key user/navigation-map (kbd "b") 'tern-pop-find-definition)
    (define-key user/documentation-map (kbd "d") 'tern-get-docs)
    (tern-ac-setup))
  ;; Configure autocompletion
  (set (make-local-variable 'ac-auto-start) 3)
  (set (make-local-variable 'ac-auto-show-menu) t))


(defun user/js-mode-hook ()
  "JS mode hook."
  (user/js-mode-common-hook))


(defun user/js2-mode-hook ()
  "JS2 mode hook."
  (user/js-mode-common-hook)
  ;; Enable js2-mode
  (js2-mode)
  ;; Enable smart indentation
  (smart-tabs-mode t)
  (smart-tabs-advice js2-indent-line js2-basic-offset)
  ;; Enable Flycheck
  (flycheck-mode t))


(defun user/javascript-mode-cedet-hook ()
  "JavaScript CEDET support hook."
  (user/cedet-hook)
  (require 'semantic/wisent/javascript))


(defun user/js2-mode-init ()
  "Initialize js2 mode."
  (setq-default
   ;; Configure indentation
   js2-indent-on-enter-key t
   js2-auto-indent-p t
   js2-bounce-indent-p t
   ;; Idle timeout before reparsing buffer
   js2-idle-timer-delay 0.5
   ;; Do not load browser-specific functions
   js2-include-browser-externs nil
   ;; Support Node.js
   js2-include-node-externs t
   js2-skip-preprocessor-directives t
   ;; Disable error parsing in favor of Flycheck
   js2-show-parse-errors nil
   js2-strict-missing-semi-warning nil)

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

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.js"))

  (add-hook 'js-mode-hook 'user/js-mode-hook)
  (add-hook 'js2-mode-hook 'user/js2-mode-hook)

  (add-auto-mode 'js2-mode "\\.js$")
  (add-auto-mode 'javascript-mode "\\.json$")
  (add-magic-mode 'js2-mode "#!/usr/bin/env node"))


(defun user/javascript-mode-init ()
  "Initialize JavaScript mode."
  (require-package '(:name js2-mode :after (user/js2-mode-init)))
  (require-package '(:name tern)))

(user/javascript-mode-init)


(provide 'modes/javascript)
;;; javascript.el ends here
