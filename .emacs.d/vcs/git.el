;;; (Initialization) ;;;
(require-package (:name magit))
(require-package (:name magithub))
(require-package (:name git-gutter-fringe))
(require-package (:name git-messenger
                        :type github
                        :pkgname "syohex/emacs-git-messenger"
                        :depends (popup)))
(require-package (:name yagist
                        :type elpa
                        :repo ("melpa" . "http://melpa.milkbox.net/packages/")
                        :depends (json)))


(setq
 git-gutter-fr:side 'left-fringe
 git-messenger:show-detail t)


;;; (Bindings) ;;;
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)


;;; (Functions) ;;;
;; Full screen magit status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (when (get-register :magit-fullscreen)
    (ignore-errors
      (jump-to-register :magit-fullscreen))))


(provide 'vcs/git)
