;;; bindings.el --- Smart VCS bindings
;;; Commentary:
;;; Code:

(defvar user/vcs-command-alist
  '((:Git . ((:status . magit-status)
             (:history . magit-file-log)
             (:describe . git-messenger:popup-message)
             (:gutter . git-gutter:toggle)
             (:review . (lambda ()
                          ;; Make it non-interactive to pick the current repo.
                          (gerrit-download)))
             (:add-buffer . (lambda ()
                              (magit-run-git "add" (file-truename
                                                    (buffer-file-name)))))
             (:mergetool . magit-ediff)))
    (:ClearCase . ((:status . (lambda ()
                                (vc-dir (file-name-directory
                                         (file-truename (buffer-file-name))))))
                   (:history . vc-print-log)
                   (:describe . vc-annotate)
                   (:gutter . diff-hl-margin-mode)
                   (:add-buffer . vc-register)
                   (:next-action . vc-next-action)
                   (:mergetool . vc-resolve-conflicts)))))


(defun user/vcs-command-group (file-name)
  "Get the appropriate command group for FILE-NAME."
  (let ((backend (vc-responsible-backend (file-name-directory file-name))))
    (cond ((eq backend 'Git)
           (cdr (assq :Git user/vcs-command-alist)))
          ((eq backend 'CLEARCASE)
           (cdr (assq :ClearCase user/vcs-command-alist))))))


(defun user/vcs-command (command)
  "Run VCS COMMAND on current buffer."
  (let ((group (user/vcs-command-group (file-truename (buffer-file-name)))))
    (when (and group (assq command group))
      (if (interactive-form (cdr (assq command group)))
          (call-interactively (cdr (assq command group)))
        (funcall (cdr (assq command group)))))))


(defun user/vcs-status ()
  "Execute VCS status command on the current buffer."
  (interactive)
  (user/vcs-command :status))


(defun user/vcs-history ()
  "Execute VCS history command on the current buffer."
  (interactive)
  (user/vcs-command :history))


(defun user/vcs-describe ()
  "Execute VCS describe command on the current buffer."
  (interactive)
  (user/vcs-command :describe))


(defun user/vcs-gutter ()
  "Execute VCS gutter command on the current buffer."
  (interactive)
  (user/vcs-command :gutter))


(defun user/vcs-next-action ()
  "Execute next VCS action on the current buffer."
  (interactive)
  (user/vcs-command :next-action))


(defun user/vcs-add-buffer ()
  "Add the current buffer to VCS."
  (interactive)
  (user/vcs-command :add-buffer))


(defun user/vcs-mergetool ()
  "Run VCS mergetool on the current buffer."
  (interactive)
  (user/vcs-command :mergetool))


(defun user/vcs-bindings-init ()
  "Initialize smart VCS bindings."
  ;; Need autoload for bindings to work.
  (autoload 'vc-responsible-backend "vc.el")

  ;;; (Bindings) ;;;
  (user/bind-key-global :vcs :status 'user/vcs-status)
  (user/bind-key-global :vcs :history 'user/vcs-history)
  (user/bind-key-global :vcs :describe 'user/vcs-describe)
  (user/bind-key-global :vcs :gutter 'user/vcs-gutter)

  (user/bind-key-global :vcs :next-action 'user/vcs-next-action)
  (user/bind-key-global :vcs :add-buffer 'user/vcs-add-buffer)
  (user/bind-key-global :vcs :mergetool 'user/vcs-mergetool))

(user/vcs-bindings-init)


(provide 'vcs/bindings)
;;; bindings.el ends here
