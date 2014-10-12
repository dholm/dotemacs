;;; frames.el --- Configure behavior of Emacs frames
;;; Commentary:
;;; Code:

(defun user/transpose-frame-init ()
  "Initialize transpose-frame."
  ;;; (Bindings) ;;;
  (user/bind-key-global :emacs :flip-frame 'flip-frame)
  (user/bind-key-global :emacs :flop-frame 'flop-frame)
  (user/bind-key-global :emacs :rotate-frame-forward 'rotate-frame-clockwise)
  (user/bind-key-global :emacs :rotate-frame-backward 'rotate-frame-anticlockwise))


(defun user/frames-init ()
  "Initialize Emacs frames."
  (setq-default
   ;; Do not show the splash screen or message
   inhibit-startup-screen t
   inhibit-startup-echo-area-message t
   ;; Inhibit GUI features
   use-file-dialog nil
   user-dialog-box nil
   ;; Only split frame if it occupies at least 2/3 of the screen width.
   split-width-threshold (* (/ (window-total-width (frame-root-window)) 3) 2)
   ;; Don't split frames horizontally.
   split-height-threshold nil)

  ;; Remove all the mouse-assisted frames
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

  ;;; (Bindings) ;;;
  (when (display-graphic-p)
    (user/bind-key-global :emacs :fullscreen 'toggle-frame-fullscreen))

  (user/bind-key-global :emacs :grow-vertical 'enlarge-window)
  (user/bind-key-global :emacs :shrink-vertical 'shrink-window)
  (user/bind-key-global :emacs :grow-horizontal 'enlarge-window-horizontally)
  (user/bind-key-global :emacs :shrink-horizontal 'shrink-window-horizontally)

  ;;; (Packages) ;;;
  (require-package '(:name fullframe))
  (require-package '(:name transpose-frame :after (user/transpose-frame-init))))

(user/frames-init)


(provide 'ux/frames)
;;; frames.el ends here
