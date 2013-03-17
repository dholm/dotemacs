;; (Utilities) ;;

;; Close all open buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;; When using profile-dotemacs start with init.el
;; To profile run emacs (-Q) -l ~/.emacs.d/utilities/profile-dotemacs/profile-dotemacs.el -f profile-dotemacs
(setq profile-dotemacs-file "~/.emacs.d/init.el")


;; Load all utilities
(load "~/.emacs.d/utilities/cedet.el")
(load "~/.emacs.d/utilities/ecb.el")
(load "~/.emacs.d/utilities/auto-complete.el")
(load "~/.emacs.d/utilities/xcscope.el")
(load "~/.emacs.d/utilities/dtrt-indent.el")
(load "~/.emacs.d/utilities/browse-kill-ring.el")
(load "~/.emacs.d/utilities/smart-tab.el")
(load "~/.emacs.d/utilities/outlookedit.el")
(load "~/.emacs.d/utilities/lusty-emacs.el")
(load "~/.emacs.d/utilities/windmove.el")
(load "~/.emacs.d/utilities/undo-tree.el")
(load "~/.emacs.d/utilities/multi-term.el")
(load "~/.emacs.d/utilities/uniquify.el")
(load "~/.emacs.d/utilities/tabbar.el")
(load "~/.emacs.d/utilities/saveplace.el")
(load "~/.emacs.d/utilities/savehist.el")
(load "~/.emacs.d/utilities/deft.el")
(load "~/.emacs.d/utilities/vlf.el")
(load "~/.emacs.d/utilities/sunrise-commander.el")
(load "~/.emacs.d/utilities/flymake.el")
(load "~/.emacs.d/utilities/wc-mode.el")
(load "~/.emacs.d/utilities/elim.el")
