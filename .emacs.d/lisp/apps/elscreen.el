;;; elscreen.el --- Emacs window session manager
;;; Commentary:
;;; Code:

(defun user/elscreen-create (&optional name)
  "Create a new screen called NAME."
  (interactive (list (read-from-minibuffer "New screen name: ")))
  (elscreen-create)
  (elscreen-screen-nickname name))


(defun user/elscreen-persist-init ()
  "Initialize ElScreen Persist."
  (setq-default
   ;; Path to elscreen persist storage.
   elscreen-persist-file (path-join *user-cache-directory* "elscreen-persist"))

  (after-load 'elscreen
    (elscreen-persist-mode t)))


(defun user/elscreen-init ()
  "Initialize ElScreen."
  (setq-default
   ;; Don't show tabs in header line.
   elscreen-display-tab nil
   ;; Don't display icons.
   elscreen-tab-display-kill-screen nil)

  (elscreen-start)

  ;;; (Bindings) ;;;
  (define-key elscreen-map (kbd "c") 'user/elscreen-create)
  (define-key elscreen-map (kbd "C-z") 'user/iconify-or-deiconify-frame)
  (define-key elscreen-map (kbd "SPC") 'helm-elscreen)
  (when (feature-p 'zoom-window)
    (define-key elscreen-map (kbd "z") 'zoom-window-zoom)))

(require-package '(:name elscreen :after (user/elscreen-init)))
(require-package '(:name elscreen-persist :after (user/elscreen-persist-init)))
(require-package '(:name zoom-window))


(provide 'apps/elscreen)
;;; elscreen.el ends here
