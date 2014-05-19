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
  (with-feature 'semantic/wisent/javascript
    (user/cedet-hook)))


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

  (add-hook 'js3-mode-hook 'user/js3-mode-hook)
  (add-auto-mode 'js3-mode "\\.js$")
  (add-magic-mode 'js3-mode "#!/usr/bin/env node"))


(defun user/javascript-mode-init ()
  "Initialize JavaScript mode."
  (add-hook 'javascript-mode-hook 'user/javascript-mode-hook)
  (add-auto-mode 'javascript-mode "\\.json$")

  (require-package '(:name js3-mode :after (user/js3-mode-init))))

(user/javascript-mode-init)


(provide 'modes/javascript)
;;; javascript.el ends here
