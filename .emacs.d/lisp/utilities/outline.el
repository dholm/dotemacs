;;; outline.el --- Outline mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--outline-mode-hook ()
  "Hook for outline mode."
  ;;; (Bindings) ;;;
  (when (feature-p 'outline-magic)
    (user/bind-key-local :nav :context-cycle 'outline-cycle)
    (user/bind-key-local :nav :context-up 'outline-move-subtree-up)
    (user/bind-key-local :nav :context-down 'outline-move-subtree-down)
    (user/bind-key-local :code :context-promote 'outline-promote)
    (user/bind-key-local :code :context-demote 'outline-demote))
  (user/bind-key-local :nav :context-forward 'outline-next-heading)
  (user/bind-key-local :nav :context-backward 'outline-previous-heading))

(use-package outline
  :defer
  :init
  (add-hook 'outline-mode-hook 'user--outline-mode-hook)
  (add-hook 'outline-minor-mode-hook 'user--outline-mode-hook)
  :config
  (use-package outline-magic
    :defer)

  (use-package outshine
    :pin "MELPA"
    :init
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
    :config
    (validate-setq
     ;; Enable org-mode-like speed keys.
     outshine-use-speed-commands t)

    (use-package navi-mode
      :pin "MELPA"
      :config
      (setf
       ;; Add "use-package" lines to `navi-keywords'.
       (cdr (assoc :ALL (cdr (assoc "emacs-lisp" navi-keywords))))
       "^[[:space:]]*(\\(use-package\\|\\(cl-\\)\\{0,1\\}def[a-z]+\\)\\*? ")

      (use-package helm-navi))))


(provide 'utilities/outline)
;;; outline.el ends here
