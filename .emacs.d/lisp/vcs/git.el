;;; git --- Git integration
;;; Commentary:
;;; Code:

(defun user/git-gutter-init ()
  "Initialize git gutter."
  ;;; (Bindings) ;;;
  (define-key user/vcs-map (kbd "g") 'git-gutter:toggle))


(defun user/git-gutter-fringe-init ()
  "Initialize git gutter fringe."
  (setq-default git-gutter-fr:side 'left-fringe))


(defun user/magit-mode-hook ()
  "Magit mode hook."
  ;; Ignore change in whitespace by default.
  (add-to-list 'magit-diff-options "--ignore-all-space"))

(defun user/magit-init ()
  "Initialize Magit."
  (setq-default
   ;; Do not save buffers
   magit-save-some-buffers nil
   ;; Automatically show process buffer if git takes too long to execute
   magit-process-popup-time 30
   ;; Show fine differences for currently selected hunk
   magit-diff-refine-hunk t
   ;; Use ido for user input
   magit-completing-read-function 'magit-ido-completing-read)

  ;; Full frame Magit status
  (el-get-eval-after-load 'fullframe
    (fullframe magit-status magit-mode-quit-window nil))

  (add-hook 'magit-mode-hook 'user/magit-mode-hook)

  ;;; (Bindings) ;;;
  (define-key user/vcs-map (kbd "s") 'magit-status)

  ;;; (Functions) ;;;
  (defun magit-quit-session ()
    "Restore the previous window configuration and kill the magit buffer."
    (interactive)
    (kill-buffer)
    (when (get-register :magit-fullscreen)
      (ignore-errors
        (jump-to-register :magit-fullscreen)))))


(defun user/git-messenger-init ()
  "Initialize git messenger."
  (setq-default git-messenger:show-detail t)
  (define-key user/vcs-map (kbd "d") 'git-messenger:popup-message))


(defun user/git-init ()
  "Initialize Git support."
  ;;; (Packages) ;;;
  (require-package '(:name magit :after (user/magit-init)))
  (require-package '(:name git-modes))
  (require-package '(:name git-gutter :after (user/git-gutter-init)))
  (when (display-graphic-p)
    (require-package '(:name git-gutter-fringe :after (user/git-gutter-fringe-init))))
  (require-package '(:name git-messenger :after (user/git-messenger-init))))

(when *has-git*
  (user/git-init))


(provide 'vcs/git)
;;; git.el ends here
