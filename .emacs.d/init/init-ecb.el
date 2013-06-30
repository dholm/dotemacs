;; Emacs Code Browser
(require 'ecb)
(require 'ecb-autoloads)


;; (Basic Configuration) ;;
(setq stack-trace-on-error nil)
(custom-set-variables
 '(ecb-options-version "2.40"))


;; (ECB Layout) ;;
(setq ecb-layout-name "left6")
(setq ecb-show-sources-in-directories-buffer 'always)


;; (Helper Functions) ;;
;;; replacement for built-in ecb-deactive, ecb-hide-ecb-windows and
;;; ecb-show-ecb-windows functions
;;; since they hide/deactive ecb but not restore the old windows for me
(defun dholm/ecb-deactivate ()
  "Deactive ecb and then split emacs into 2 windows that contain 2 most recent buffers"
  (interactive)
  (ecb-deactivate)
  (split-window-right)
  (switch-to-next-buffer)
  (other-window 1))
(defun dholm/ecb-hide-ecb-windows ()
  "Hide ecb and then split emacs into 2 windows that contain 2 most recent buffers"
  (interactive)
  (ecb-hide-ecb-windows)
  (split-window-right)
  (switch-to-next-buffer)
  (other-window 1))
(defun dholm/ecb-show-ecb-windows ()
  "Show ecb windows and then delete all other windows except the current one"
  (interactive)
  (ecb-show-ecb-windows)
  (delete-other-windows))


;; (Key Bindings) ;;
;;; activate and deactivate ecb
(global-set-key (kbd "C-x C-;") 'ecb-activate)
(global-set-key (kbd "C-x C-'") 'dholm/ecb-deactivate)
;;; show/hide ecb window
(global-set-key (kbd "C-;") 'dholm/ecb-show-ecb-windows)
(global-set-key (kbd "C-'") 'dholm/ecb-hide-ecb-windows)
;;; quick navigation between ecb windows
(global-set-key (kbd "C-)") 'ecb-goto-window-edit1)
(global-set-key (kbd "C-!") 'ecb-goto-window-directories)
(global-set-key (kbd "C-@") 'ecb-goto-window-sources)
(global-set-key (kbd "C-#") 'ecb-goto-window-methods)
(global-set-key (kbd "C-$") 'ecb-goto-window-compilation)
