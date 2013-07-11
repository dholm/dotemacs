;;; (Initialization) ;;;


(setq-default
 ;; Put the repository in the data directory
 bm-repository-file (path-join *user-data-directory* "bm-repository")
 ;; Make bookmarks persistent by default
 bm-buffer-persistence t
 ;; Make sure the repository is loaded as early as possible
 bm-restore-repository-on-load t)


;; The hooks are not autoloadable so bring in bm before registering them
(require 'bm)


;; Loading the repository from file when on start up.
(add-hook' after-init-hook 'bm-repository-load)
;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)
;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)
;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when Emacs is killed, so we must save all
;; bookmarks first.
(add-hook 'kill-emacs-hook '(lambda ()
                              (bm-buffer-save-all)
                              (bm-repository-save)))
;; Update bookmark repository when saving the file.
(add-hook 'after-save-hook 'bm-buffer-save)
;; Restore bookmarks when buffer is reverted.
(add-hook 'after-revert-hook 'bm-buffer-restore)
;; make sure bookmarks is saved before check-in (and revert-buffer)
(add-hook 'vc-before-checkin-hook 'bm-buffer-save)


;;; (Bindings) ;;;
(global-set-key (kbd "C-x r m") 'bm-toggle)
(global-set-key (kbd "C-x r a") 'bm-bookmark-annotate)
(global-set-key (kbd "C-x r l") 'bm-show-all)
(global-set-key (kbd "C-x r b") 'bm-next)

(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
