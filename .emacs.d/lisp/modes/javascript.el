;;; javascript.el --- initializes JavaScript modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--javascript-mode-common-hook ()
  "JavaScript common mode hook."
  ;; Load CEDET
  (user--cedet-hook)

  (user/gnu-global-enable)

  (tern-mode t))


(defun user--javascript-mode-hook ()
  "JavaScript mode hook."
  (user--javascript-mode-common-hook))


(defun user--inferior-js-mode-hook ()
  "Inferior JavaScript mode hook."
  ;; Support ANSI colors.
  (ansi-color-for-comint-mode-on))


(defun user--js2-mode-hook ()
  "JS2 mode hook."
  (user--javascript-mode-common-hook)
  ;; Enable smart indentation
  (smart-tabs-mode t)
  ;; Enable Flycheck
  (flycheck-mode t))


(use-package js
  :defer
  :hook ((javascript-mode-hook . user--javascript-mode-hook)
         (inferior-js-mode-hook . user--inferior-javascript-mode-hook)))

(use-package js2-mode
  :defer
  :mode "\.js$"
  :magic "#!/usr/bin/env node"
  :hook (js2-mode-hook . user--js2-mode-hook)
  :config
  (validate-setq
   ;; Configure indentation
   js2-enter-indents-newline t
   js2-auto-indent-p t
   ;; Idle timeout before reparsing buffer
   js2-idle-timer-delay 0.5
   ;; Disable error parsing in favor of Flycheck
   js2-strict-missing-semi-warning nil)

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

  (use-package prettier-js)

  (use-package lsp-javascript-typescript
    :if (executable-find "javascript-typescript-langserver")
    :hook (js2-mode-hook . lsp-javascript-typescript-enable))

  (use-package helm-js-codemod
    :if (executable-find "jscodeshift"))

  (use-package xref-js2
    :init
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))


(provide 'modes/javascript)
;;; javascript.el ends here
