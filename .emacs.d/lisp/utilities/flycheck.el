;;; flycheck.el --- flycheck configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--flycheck-mode-hook ()
  "Flycheck mode hook."
  ;;; (Bindings) ;;;
  (user/bind-key-local :code :warnings/errors 'helm-flycheck)
  (user/bind-key-local :nav :next 'flycheck-next-error))


(defun user/ede-flycheck-setup ()
  "Configure `flycheck' using `ede'."
  (when (and ede-object
             (slot-exists-p ede-object 'compilation)
             (oref ede-object compilation))
    (let* ((comp (oref ede-object compilation))
           (cmd (get-command-line comp)))
      (setq
       flycheck-clang-includes (get-includes comp)
       flycheck-clang-definitions (get-defines comp)
       flycheck-clang-include-path (oref comp include-path-common))

      (when (string-match " -std=\\([^ ]+\\)" cmd)
        (setq
         flycheck-clang-language-standard (match-string 1 cmd)
         flycheck-gcc-language-standard (match-string 1 cmd)))
      (when (string-match " -stdlib=\\([^ ]+\\)" cmd)
        (setq flycheck-clang-standard-library (match-string 1 cmd)))
      (when (string-match " -fms-extensions " cmd)
        (setq flycheck-clang-ms-extensions t))
      (when (string-match " -fno-exceptions " cmd)
        (setq
         flycheck-clang-no-exceptions t
         flycheck-gcc-no-exceptions t))
      (when (string-match " -fopenmp" cmd)
        (setq flycheck-gcc-openmp t))
      (when (string-match " -fno-rtti " cmd)
        (setq
         flycheck-clang-no-rtti t
         flycheck-gcc-no-rtti t))
      (when (string-match " -fblocks " cmd)
        (setq flycheck-clang-blocks t)))))


(use-package flycheck
  :init
  (add-hook 'flycheck-mode-hook 'user--flycheck-mode-hook)
  (add-hook 'ede-minor-mode-hook 'user/ede-flycheck-setup)
  (add-hook 'ede-compdb-project-rescan-hook 'user/ede-flycheck-setup)

  (global-flycheck-mode t)
  :config
  (validate-setq
   ;; Wait five seconds before starting checker
   flycheck-idle-change-delay 5.0)

  (after-load 'popwin
    ;; Use popwin for Flycheck error list.
    (push '(flycheck-error-list-mode :stick t) popwin:special-display-config))

  ;;; (Packages) ;;;
  (use-package flycheck-pos-tip
    :if window-system
    :config
    ;; Make sure flycheck-pos-tip is loaded.
    (require 'flycheck-pos-tip nil t))

  (use-package helm-flycheck))


(provide 'utilities/flycheck)
;;; flycheck.el ends here
