;; (Utilities) ;;

;; Close all open buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;; Load all utilities
(load "~/.emacs.d/utilities/compile.el")
(load "~/.emacs.d/utilities/xcscope.el")
(load "~/.emacs.d/utilities/outlookedit.el")
(load "~/.emacs.d/utilities/windmove.el")
(load "~/.emacs.d/utilities/uniquify.el")
(load "~/.emacs.d/utilities/tabbar.el")
(load "~/.emacs.d/utilities/saveplace.el")
(load "~/.emacs.d/utilities/savehist.el")
