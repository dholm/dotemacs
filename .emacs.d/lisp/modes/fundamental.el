;;; fundamental.el --- Base mode of all other major modes
;;; Commentary:
;;; Code:

(defun user/fundamental-mode-hook ()
  "Fundamental mode hook."
  ;; Automatically break long lines.
  (auto-fill-mode t)

  ;; Enable whitespace mode globally.
  (whitespace-mode t)

  ;; Enable dtrt-indent to attempt to identify the indentation rules used.
  (after-load 'dtrt-indent
    (dtrt-indent-mode t))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :align 'align-current)
  (user/bind-key-local :nav :functions/toc 'helm-imenu))


(defun user/rainbow-delimiters-init ()
  "Initialize rainbow delimiters."
  (global-rainbow-delimiters-mode t))


(defun user/mic-paren-init ()
  "Initialize mic-paren."
  (paren-activate))


(defun user/fixmee-mode-init ()
  "Initialize fixmee mode."
  (setq-default
   ;; Fixmee uses pcache that needs a cache store.
   pcache-directory (path-join *user-cache-directory* "pcache/")
   ;; Set the correct bindings.
   fixmee-view-listing-keystrokes `(,(user/get-key :code :todos)))

  (after-load 'popwin
    ;; Use popwin for Fixmee list.
    (push '(fixmee--listview-mode :stick t) popwin:special-display-config))

  (global-fixmee-mode t))


(defun user/fundamental-mode-init ()
  "Initialize Emacs fundamental mode."
  (setq-default
   ;; Indent using spaces by default.
   indent-tabs-mode nil
   ;; When using fill-paragraph or auto-fill-mode break lines at 80 characters by
   ;; default.
   fill-column 80)

  (after-load 'diminish
    ;; Diminish common modes.
    (diminish 'abbrev-mode)
    (diminish 'auto-fill-function))

  ;;; (Packages) ;;;
  (require-package '(:name fixmee :after (user/fixmee-mode-init)))
  (require-package '(:name rainbow-delimiters
                           :after (user/rainbow-delimiters-init)))
  (require-package '(:name mic-paren :after (user/mic-paren-init))))

(user/fundamental-mode-init)


(provide 'modes/fundamental)
;;; fundamental.el ends here
