;;; frames.el --- Configure behavior of Emacs frames
;;; Commentary:
;;; Code:

(defun user/frames-init ()
  "Initialize Emacs frames."
  (setq-default
   ;; Do not show the splash screen or message
   inhibit-startup-screen t
   inhibit-startup-echo-area-message t
   ;; Inhibit GUI features
   use-file-dialog nil
   user-dialog-box nil)

  ;; Remove all the mouse-assisted frames
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

  ;;; (Bindings) ;;;
  (global-set-key (kbd "C-c <up>") 'shrink-window)
  (global-set-key (kbd "C-c <down>") 'enlarge-window)
  (global-set-key (kbd "C-c <left>") 'shrink-window-horizontally)
  (global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)

  ;;; (Packages) ;;;
  (require-package '(:name fullframe)))

(user/frames-init)


(provide 'ux/frames)
;;; frames.el ends here
