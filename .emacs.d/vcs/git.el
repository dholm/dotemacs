;;; (Initialization) ;;;
(require-package '(:name magit :after (dholm/magit-init)))
(require-package '(:name magithub))
(require-package '(:name git-gutter-fringe :after (dholm/git-gutter-fringe-init)))
(require-package '(:name git-messenger
			 :after (dholm/git-messenger-init)
			 :type github
			 :pkgname "syohex/emacs-git-messenger"
			 :depends (popup)))
(require-package '(:name yagist
			 :type elpa
			 :repo ("melpa" . "http://melpa.milkbox.net/packages/")
			 :depends (json)))


(defun dholm/git-gutter-fringe-init ()
  (setq
   git-gutter-fr:side 'left-fringe
   git-messenger:show-detail t))


(defun dholm/magit-init ()
  ;; Full screen magit status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (global-set-key (kbd "C-c v s") 'magit-status))


(defun dholm/git-messenger-init ()
  (global-set-key (kbd "C-c d v") 'git-messenger:popup-message))


;;; (Functions) ;;;
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (when (get-register :magit-fullscreen)
    (ignore-errors
      (jump-to-register :magit-fullscreen))))


(provide 'vcs/git)
