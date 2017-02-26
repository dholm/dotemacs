;;; ecb --- Emacs Code Browser
;;; Commentary:
;;; Code:

(defvar user/ecb-active nil "Current state of ECB.")


(defun user--ecb-activate-hook ()
  "ECB activation hook."
  (setq user/ecb-active t)

  ;; Popwin conflicts with ECB.
  (popwin-mode -1)

  ;; Close compile window if open
  (when (ecb-compile-window-live-p)
    (ecb-toggle-compile-window))

  ;;; (Bindings) ;;;
  (user/bind-key-local :basic :zoom 'ecb-toggle-ecb-windows)

  (user/bind-key-local :nav :context 'ecb-goto-window-edit1)
  (user/bind-key-local :basic :open-file-context 'ecb-goto-window-directories)
  (user/bind-key-local :nav :history 'ecb-goto-window-history)
  (user/bind-key-local :nav :functions/toc 'ecb-goto-window-methods)
  (user/bind-key-local :code :compilation-result 'user/ecb-toggle-compile-window))


(defun user--ecb-deactivate-hook ()
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

(when (version<= emacs-version "24.4")
  (use-package ecb
    :defer
    :init
    (add-hook 'ecb-activate-hook 'user--ecb-activate-hook)
    (add-hook 'ecb-deactivate-hook 'user--ecb-deactivate-hook)
    (user/bind-key-global :util :ecb-toggle 'user/ecb-toggle-active)
    :config
    ;; ECB version checking code is very old so that it thinks that the latest
    ;; CEDET/Emacs is not new enough when in fact it is years newer than the
    ;; latest version that it is aware of.  So simply bypass the version check.
    (validate-setq
     ecb-version-check nil
     ecb-tip-of-the-day nil
     ;; ECB layout.
     ecb-layout-name "left6"
     ecb-layout-window-sizes '(("left6"
                                (ecb-directories-buffer-name 0.17 . 0.41)
                                (ecb-sources-buffer-name 0.17 . 0.21)
                                (ecb-methods-buffer-name 0.17 . 0.41)
                                (ecb-history-buffer-name 0.17 . 0.35)))
     ecb-show-sources-in-directories-buffer 'always
     ecb-compile-window-height 12)

    (defadvice ecb-check-requirements (around no-version-check activate compile)
      "AROUND NO-VERSION-CHECK ACTIVATE COMPILE"
      (if (or (< emacs-major-version 23)
              (and (= emacs-major-version 23)
                   (< emacs-minor-version 3)))
          ad-do-it))

    (when (display-graphic-p)
      (after-load 'ecb-face
        ;; Use a slightly smaller face for the ECB tree-buffers.
        (set-face-attribute 'ecb-default-general-face nil :height 0.8)))))


(provide 'utilities/ecb)
;;; ecb.el ends here
