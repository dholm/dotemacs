;;; javascript.el --- initializes JavaScript modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--javascript-mode-common-hook ()
  "JavaScript common mode hook."
  ;; Load CEDET
  (user--javascript-mode-cedet-hook)

  (user/gnu-global-enable)

  (tern-mode t))


(defun user--javascript-mode-hook ()
  "JavaScript mode hook."
  (user--javascript-mode-common-hook))


(defun user--inferior-js-mode-hook ()
  "Inferior JavaScript mode hook."
  ;; Support ANSI colors.
  (ansi-color-for-comint-mode-on))


(defun user--js3-mode-hook ()
  "JS3 mode hook."
  (user--javascript-mode-common-hook)
  ;; Enable smart indentation
  (smart-tabs-mode t)
  ;; Enable Flycheck
  (flycheck-mode t))


(defun user--javascript-mode-cedet-hook ()
  "JavaScript CEDET support hook."
  (with-feature 'semantic/wisent/javascript
    (wisent-javascript-setup-parser)
    (user--cedet-hook)))

(use-package js
  :defer
  :init
  (add-hook 'javascript-mode-hook 'user--javascript-mode-hook)
  (add-hook 'inferior-js-mode-hook 'user--inferior-javascript-mode-hook))

(use-package js3-mode
  :defer
  :mode "\.js$"
  :magic "#!/usr/bin/env node"
  :init
  (add-hook 'js3-mode-hook 'user--js3-mode-hook)
  :config
  (validate-setq
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

  (use-package js-comint
    :config
    (validate-setq
     ;; Set JavaScript inferior.
     inferior-js-program-command
     (cond
      ((executable-find "js") (executable-find "js"))
      ((executable-find "node")
       (concat (executable-find "node") " --interactive"))
      (t "java org.mozilla.javascript.tools.shell.Main")))

    ;; Workaround for Node.js prompt.
    (setenv "NODE_NO_READLINE" "1"))

  (use-package prettier-js))


(provide 'modes/javascript)
;;; javascript.el ends here
