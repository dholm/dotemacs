;;; ecb --- Emacs Code Browser
;;; Commentary:
;;; Code:

(defvar user/ecb-active nil "Current state of ECB.")


(defun user/ecb-activate-hook ()
  "ECB activation hook."
  (setq user/ecb-active t)

  ;; Popwin conflicts with ECB.
  (popwin-mode -1)

  ;; Close compile window if open
  (when (ecb-compile-window-live-p)
    (ecb-toggle-compile-window))

  ;;; (Bindings) ;;;
  (define-key user/view-map (kbd "z") 'ecb-toggle-ecb-windows)

  (define-key user/view-map (kbd "e") 'ecb-goto-window-edit1)
  (define-key user/view-map (kbd "d") 'ecb-goto-window-directories)
  (define-key user/view-map (kbd "h") 'ecb-goto-window-history)
  (define-key user/view-map (kbd "m") 'ecb-goto-window-methods)
  (define-key user/view-map (kbd "c") 'user/ecb-toggle-compile-window))


(defun user/ecb-deactivate-hook ()
  "ECB deactivation hook."
  (setq user/ecb-active nil)
  (popwin-mode t))


(defun user/ecb-toggle-active ()
  "Toggle ECB state."
  (interactive)
  (if user/ecb-active
      (ecb-deactivate)
    (ecb-activate)))


(defun user/ecb-toggle-compile-window ()
  "Toggle the presence of the ECB compilation window."
  (interactive)
  (if (and (ecb-compile-window-live-p)
         (not (ecb-point-in-compile-window)))
      (ecb-goto-window-compilation)
    (progn
      (ecb-toggle-compile-window)
      (when (ecb-compile-window-live-p)
        (ecb-goto-window-compilation)))))


(defun user/ecb-init ()
  "Initialize Emacs code browser."
  (require 'ecb-autoloads)

  ;; ECB version checking code is very old so that it thinks that the latest
  ;; CEDET/Emacs is not new enough when in fact it is years newer than the
  ;; latest version that it is aware of.  So simply bypass the version check.
  (setq-default
   ecb-version-check nil
   ecb-tip-of-the-day nil)
  (defadvice ecb-check-requirements (around no-version-check activate compile)
    "AROUND NO-VERSION-CHECK ACTIVATE COMPILE"
    (if (or (< emacs-major-version 23)
           (and (= emacs-major-version 23)
              (< emacs-minor-version 3)))
        ad-do-it))

  ;; ECB layout
  (setq-default
   ecb-layout-name "left6"
   ecb-layout-window-sizes '(("left6"
			      (ecb-directories-buffer-name 0.17 . 0.6428571428571429)
			      (ecb-sources-buffer-name 0.17 . 0.3392857142857143)
			      (ecb-methods-buffer-name 0.25 . 0.6428571428571429)
			      (ecb-history-buffer-name 0.25 . 0.3392857142857143)))
   ecb-show-sources-in-directories-buffer 'always
   ecb-compile-window-height 12)

  (add-hook 'ecb-activate-hook 'user/ecb-activate-hook)
  (add-hook 'ecb-deactivate-hook 'user/ecb-deactivate-hook)

  ;;; (Bindings) ;;;
  (define-key user/utilities-map (kbd "e") 'user/ecb-toggle-active)

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(ecb-default-highlight-face ((t (:foreground ,solarized-bg :background ,blue))))
         '(ecb-history-bucket-node-dir-soure-path-face ((t (:inherit ecb-history-bucket-node-face
                                                                     :foreground ,yellow))))
         '(ecb-source-in-directories-buffer-face ((t (:inherit ecb-directories-general-face
                                                               :foreground ,solarized-fg))))
         '(ecb-history-dead-buffer-face ((t (:inherit ecb-history-general-face
                                                      :foreground ,solarized-comment))))
         '(ecb-directory-not-accessible-face ((t (:inherit ecb-directories-general-face
                                                           :foreground ,solarized-comment))))
         '(ecb-bucket-node-face ((t (:inherit ecb-default-general-face :foreground ,blue
                                              :weight normal))))
         '(ecb-tag-header-face ((t (:background ,solarized-hl))))
         '(ecb-analyse-bucket-element-face ((t (:inherit ecb-analyse-general-face :foreground ,green))))
         '(ecb-directories-general-face ((t (:inherit ecb-default-general-face :height 1.0))))
         '(ecb-method-non-semantic-face ((t (:inherit ecb-methods-general-face :foreground ,cyan))))
         '(ecb-mode-line-prefix-face ((t (:foreground ,green))))
         '(ecb-tree-guide-line-face ((t (:inherit ecb-default-general-face :foreground ,solarized-hl
                                                  :height 1.0)))))))))

(require-package '(:name ecb :after (user/ecb-init)))


(provide 'utilities/ecb)
;;; ecb.el ends here
