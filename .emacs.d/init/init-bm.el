;;; (Initialization) ;;;

;; Make bookmarks persistent
(setq-default bm-buffer-persistence t)

;; The hooks are not autoloadable so bring in bm before registering them
(require 'bm)

(add-hook' after-init-hook 'bm-repository-load)
(add-hook 'find-file-hooks 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'kill-emacs-hook '(lambda ()
                              (bm-buffer-save-all)
                              (bm-repository-save)))


;;; (Bindings) ;;;
(global-set-key (kbd "C-x r m") 'bm-toggle)
(global-set-key (kbd "C-x r a") 'bm-bookmark-annotate)
(global-set-key (kbd "C-x r l") 'bm-show-all)
(global-set-key (kbd "C-x r b") 'bm-next)

(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
