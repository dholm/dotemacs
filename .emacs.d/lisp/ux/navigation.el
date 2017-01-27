;;; navigation.el --- Set up Emacs buffer navigation
;;; Commentary:
;;; Code:

(defun user/scroll-up ()
  "Scroll page up without moving point."
  (interactive)
  (scroll-down 1))


(defun user/scroll-down ()
  "Scroll page down without moving point."
  (interactive)
  (scroll-up 1))


(defun user--ace-jump-mode-config ()
  "Initialize ace jump mode."
  ;;; (Bindings) ;;;
  (user/bind-key-global :util :ace-jump-mode 'ace-jump-mode))


(defun user--smart-forward-config ()
  "Initialize smart-forward."
  (user/bind-key-global :nav :context-forward 'smart-forward)
  (user/bind-key-global :nav :context-backward 'smart-backward)
  (user/bind-key-global :nav :context-up 'smart-up)
  (user/bind-key-global :nav :context-down 'smart-down))


(defun user--navigation-config ()
  "Set up Emacs buffer navigation."
  ;; Enable mouse in iTerm2
  (when (eq system-type 'darwin)
    (with-feature 'mouse
      (xterm-mouse-mode t)
      (defun track-mouse (e))))

  ;;; (Bindings) ;;;
  (user/bind-key-global :nav :scroll-up 'user/scroll-up)
  (user/bind-key-global :nav :scroll-down 'user/scroll-down)
  (user/bind-key-global :nav :goto-line 'goto-line)
  (user/bind-key-global :nav :go-back 'pop-global-mark)

  ;;; (Packages) ;;;
  (req-package ace-jump-mode
    :config (user--ace-jump-mode-config))
  (req-package smart-forward
    :config (user--smart-forward-config)))

(user--navigation-config)


(provide 'ux/navigation)
;;; navigation.el ends here
