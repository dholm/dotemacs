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


(defun user/ace-jump-mode-init ()
  "Initialize ace jump mode."
  ;;; (Bindings) ;;;
  (user/bind-key-global :util :ace-jump-mode 'ace-jump-mode))


(defun user/smart-forward-init ()
  "Initialize smart-forward."
  (user/bind-key-global :nav :context-forward 'smart-forward)
  (user/bind-key-global :nav :context-backward 'smart-backward)
  (user/bind-key-global :nav :context-up 'smart-up)
  (user/bind-key-global :nav :context-down 'smart-down))


(defun user/navigation-init ()
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
  (use-package ace-jump-mode
    :ensure t
    :config (user/ace-jump-mode-init))
  (use-package smart-forward
    :ensure t
    :config (user/smart-forward-init)))

(user/navigation-init)


(provide 'ux/navigation)
;;; navigation.el ends here
