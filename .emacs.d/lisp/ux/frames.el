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
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

  ;;; (Bindings) ;;;
  (user/bind-key-global :emacs :grow-vertical 'enlarge-window)
  (user/bind-key-global :emacs :shrink-vertical 'shrink-window)
  (user/bind-key-global :emacs :grow-horizontal 'enlarge-window-horizontally)
  (user/bind-key-global :emacs :shrink-horizontal 'shrink-window-horizontally)

  ;;; (Packages) ;;;
  (require-package '(:name fullframe)))

(user/frames-init)


(provide 'ux/frames)
;;; frames.el ends here
