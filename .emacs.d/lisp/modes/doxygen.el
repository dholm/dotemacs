;;; doxygen.el --- Doxygen mode support
;;; Commentary:
;;; Code:

(defgroup doxygen nil "Support for Doxygen documentation."
  :group 'convenience)

(defvar doxygen-mode nil
  "Doxygen mode enabled when non-nil.")
(make-variable-buffer-local 'doxygen-mode)

(defcustom doxygen-mode-hook nil
  "Hook to run when `doxygen-mode' is turned on."
  :group 'doxygen
  :type '(hook))


(defun user/doxygen-mode-hook ()
  "Mode hook for Doxygen."
  (with-feature 'doxymacs
    ;; Enable doxymacs font lock.
    (doxymacs-font-lock))

  ;;; (Bindings) ;;;
  (with-feature 'doc-mode
    (user/bind-key-local :code :document 'doc-mode-fix-tag-doc)))


(defun doxygen-mode (&optional arg)
  "Minor mode for Doxygen documentation, if ARG is nil the mode is toggled."
  (interactive "P")
  (setq doxygen-mode
        (if (null arg)
            (not doxygen-mode)
          (> (prefix-numeric-value arg) 0)))
  (when doxygen-mode
    (with-feature 'doxymacs
      (doxymacs-mode t))
    (with-feature 'doc-mode
      (doc-mode t)
      (after-load 'diminish
        (diminish 'doc-mode))
      ;; Disable buffer check from semantic idle scheduler since it tends to
      ;; hang Emacs for a long time.
      (remove-hook 'semantic-after-idle-scheduler-reparse-hooks
                   'doc-mode-check-buffer t))
    (run-hooks 'doxygen-mode-hook)))


(defun user/doxygen-mode-init ()
  "Initialize Doxygen mode."
  (add-hook 'doxygen-mode-hook 'user/doxygen-mode-hook)

  ;;; (Packages) ;;;
  (when (pkg-config-has-p "libxml-2.0")
    (req-package doxymacs
      :loader :el-get))
  (req-package doc-mode-nschum
    :loader :el-get))

(with-executable 'doxygen
  (user/doxygen-mode-init))


(provide 'utilities/doxygen)
;;; doxygen.el ends here
