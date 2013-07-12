;; Emacs Code Browser
(require-package (:name ecb :after (dholm/ecb-init)))

(defun dholm/ecb-init ()
  (require 'ecb-autoloads))


;; (Basic Configuration) ;;
(setq stack-trace-on-error nil
      ecb-options-version "2.40")


;; (ECB Layout) ;;
(setq
 ecb-layout-name "left7"
 ecb-layout-window-sizes '(("left7"
                            (ecb-directories-buffer-name 0.17 . 0.6428571428571429)
                            (ecb-sources-buffer-name 0.17 . 0.3392857142857143)
                            (ecb-methods-buffer-name 0.25 . 0.6428571428571429)
                            (ecb-history-buffer-name 0.25 . 0.3392857142857143)))
 ecb-show-sources-in-directories-buffer 'always
 ecb-compile-window-height 12)


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
(global-set-key (kbd "C-c 1") 'ecb-goto-window-edit1)
(global-set-key (kbd "C-c 2") 'ecb-goto-window-directories)
(global-set-key (kbd "C-c 3") 'ecb-goto-window-methods)
(global-set-key (kbd "C-c 4") 'ecb-goto-window-history)
(global-set-key (kbd "C-c 5") 'ecb-goto-window-compilation)


(provide 'utilities/ecb)
