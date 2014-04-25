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
  (user/bind-key-local :code :document 'doxygen-insert-dwim))


(defun doxygen-insert-fn (point)
  "Get insertion function at POINT."
  (let ((ctxt (semantic-analyze-current-context))
        (pfx nil)
        (tag nil)
        (class nil))
    (when ctxt
      (setq pfx (reverse (oref ctxt :prefix)))
      (while (and (not tag) pfx)
        (when (semantic-tag-p (car pfx))
          (setq tag (car pfx)))
        (setq pfx (cdr pfx))))
    (if tag
        (let ((is-member (or (eq (semantic--tag-get-property tag 'reparse-symbol)
                                 'enumsubparts)))
              (is-function (semantic-tag-of-class-p tag 'function)))
          (cond (is-member (doxymacs-insert-member-comment))
                (is-function (progn
                               (beginning-of-line)
                               (newline-and-indent)
                               (forward-line -1)
                               (indent-according-to-mode)
                               (doxymacs-insert-function-comment)))
                (t nil)))
      (let ((is-first-line (eq (count-lines 1 point) 0)))
        (cond ((region-active-p) (doxymacs-insert-grouping-comments
                                  (region-beginning) (region-end)))
              (is-first-line (doxymacs-insert-file-comment)))))))


(defun doxygen-insert-dwim ()
  "Blah."
  (interactive)
  (save-excursion
    (doxygen-insert-fn (point))))


(defun doxygen-mode (&optional arg)
  "Minor mode for Doxygen documentation, if ARG is nil the mode is toggled."
  (interactive "P")
  (setq doxygen-mode
        (if (null arg)
            (not doxygen-mode)
          (> (prefix-numeric-value arg) 0)))
  (when doxygen-mode
    (with-feature 'doxymacs
      (doxymacs-mode))
    (run-hooks 'doxygen-mode-hook)))


(defun user/doxygen-mode-init ()
  "Initialize Doxygen mode."
  (add-hook 'doxygen-mode-hook 'user/doxygen-mode-hook)

  (when (pkg-config-has-p "libxml-2.0")
    (require-package '(:name doxymacs))))

(with-executable 'doxygen
  (user/doxygen-mode-init))


(provide 'utilities/doxygen)
;;; doxygen.el ends here
