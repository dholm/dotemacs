;;; workgroups.el --- Group buffers based on related work.
;;; Commentary:
;;; Code:

(defconst *user-workgroups-session-file*
  (path-join *user-cache-directory* "workgroups"))


(defun user/workgroups-init ()
  "Initialize workgroups."
  (setq-default
   ;; Location of session cache.
   wg-session-file *user-workgroups-session-file*
   ;; Workgroups prefix key.
   wg-prefix-key (kbd "C-x x")
   ;; Save session without asking.
   wg-emacs-exit-save-behavior 'save
   wg-workgroups-mode-exit-save-behavior 'save
   ;; Display workgroup in mode line.
   wg-mode-line-display-on t
   wg-flag-modified t
   wg-mode-line-decor-left-brace "["
   wg-mode-line-decor-divider ":"
   wg-mode-line-decor-right-brace "]")

  (workgroups-mode t)
  (after-load 'diminish
    (diminish 'workgroups-mode))

  ;;; (Bindings) ;;;
  (user/bind-key-global :util :perspective 'wg-switch-to-workgroup))

(require-package '(:name workgroups2 :after (user/workgroups-init)))


(provide 'utilities/workgroups)
;;; workgroups.el ends here
