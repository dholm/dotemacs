;;; git --- Git integration
;;; Commentary:
;;; Code:

(defun dholm/git-gutter-init ()
  "Initialize git gutter."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(git-gutter:added ((t (:foreground ,green :weight bold))))
         '(git-gutter:deleted ((t (:foreground ,red :weight bold))))
         '(git-gutter:modified ((t (:foreground ,blue :weight bold))))
         '(git-gutter:unchanged ((t (:foreground ,base02 :weight bold))))))))

  ;;; (Bindings) ;;;
  (define-key dholm/vcs-map (kbd "g") 'git-gutter:toggle))


(defun dholm/git-gutter-fringe-init ()
  "Initialize git gutter fringe."
  (setq-default git-gutter-fr:side 'left-fringe)

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(git-gutter-fr:added ((t (:foreground ,green  :weight bold))))
         '(git-gutter-fr:deleted ((t (:foreground ,red :weight bold))))
         '(git-gutter-fr:modified ((t (:foreground ,blue :weight bold)))))))))


(defun dholm/magit-init ()
  "Initialize magit."
  (setq-default
   ;; Do not save buffers
   magit-save-some-buffers nil
   ;; Automatically show process buffer if git takes too long to execute
   magit-process-popup-time 10
   ;; Show fine differences for currently selected hunk
   magit-diff-refine-hunk t
   ;; Use ido for user input
   magit-completing-read-function 'magit-ido-completing-read)

  ;; Full screen magit status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(magit-section-title ((t (:foreground ,yellow :weight bold))))
         '(magit-branch ((t (:foreground ,orange :weight bold))))
         '(magit-item-highlight ((t (:background ,base02 :weight unspecified))))
         '(magit-log-author ((t (:foreground ,cyan))))
         '(magit-log-graph ((t (:foreground ,base01))))
         '(magit-log-head-label-bisect-bad ((t (:foreground ,red :box 1))))
         '(magit-log-head-label-bisect-good ((t (:foreground ,green :box 1))))
         '(magit-log-head-label-default ((t (:background ,base02 :box 1))))
         '(magit-log-head-label-local ((t (:foreground ,blue :box 1))))
         '(magit-log-head-label-patches ((t (:foreground ,red :box 1))))
         '(magit-log-head-label-remote ((t (:foreground ,green :box 1))))
         '(magit-log-head-label-tags ((t (:foreground ,yellow :box 1))))
         '(magit-log-sha1 ((t (:foreground ,yellow))))))))

  ;;; (Bindings) ;;;
  (define-key dholm/vcs-map (kbd "s") 'magit-status)

  ;;; (Functions) ;;;
  (defun magit-quit-session ()
    "Restore the previous window configuration and kill the magit buffer."
    (interactive)
    (kill-buffer)
    (when (get-register :magit-fullscreen)
      (ignore-errors
        (jump-to-register :magit-fullscreen)))))


(defun dholm/git-messenger-init ()
  "Initialize git messenger."
  (setq-default git-messenger:show-detail t)
  (define-key dholm/vcs-map (kbd "d") 'git-messenger:popup-message))


(require-package '(:name magit :after (dholm/magit-init)))
(require-package '(:name magithub))
(require-package '(:name git-gutter :after (dholm/git-gutter-init)))
(when (display-graphic-p)
  (require-package '(:name git-gutter-fringe
                           :after (dholm/git-gutter-fringe-init))))
(require-package '(:name git-messenger
			 :after (dholm/git-messenger-init)
			 :type github
			 :pkgname "syohex/emacs-git-messenger"
			 :depends (popup)))
(require-package '(:name yagist
			 :type github
			 :pkgname "mhayashi1120/yagist.el"
			 :depends (json)))


(provide 'vcs/git)
;;; git.el ends here
