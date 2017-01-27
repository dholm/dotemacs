;;; kill-yank.el --- Configuration for copying and pasting of data
;;; Commentary:
;;; Code:

(defun user--cua-mode-hook ()
  "CUA mode hook."
  (define-key cua--rectangle-keymap (kbd "C-o") nil))


(defun user--expand-region-config ()
  "Initialize expand region."
  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :selection-expand 'er/expand-region))


(defun user--multiple-cursors-config ()
  "Initialize multiple cursors."
  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :selection-next 'mc/mark-next-like-this)
  (user/bind-key-global :basic :selection-prev 'mc/mark-previous-like-this)
  (user/bind-key-global :basic :selection-all 'mc/mark-all-like-this)
  (user/bind-key-global :basic :selection-edit-lines 'mc/edit-lines))


(defun user--kill-yank-config ()
  "Initialize copy/paste."
  (setq-default
   ;; Mouse selection should not automatically go to kill ring.
   mouse-drag-copy-region nil)

  (when (eq window-system 'x)
    (setq-default
     ;; Don't inject mouse selection into X11 clipboard.
     mouse-drag-copy-region nil
     ;; Do not interact with X11 primary selection.
     x-select-enable-primary nil
     ;; Make kill/yank interact with X11 clipboard selection.
     x-select-enable-clipboard t
     ;; Active region should set primary X11 selection.
     select-active-regions t)

    ;; Set middle mouse button to paste from primary X11 selection.
    (global-set-key [mouse-2] 'mouse-yank-primary))

  ;;; (Hooks) ;;;
  (add-hook 'cua-mode-hook 'user--cua-mode-hook)

  ;; Enable CUA selection mode for nicer rectangle selection.
  (cua-selection-mode t)

  ;;; (Bindings) ;;;
  ;; Delete words with C/M-w and rebind kill/yank region to C-x C-k/C-x C-w.
  (user/bind-key-global :basic :cut-word-left 'backward-kill-word)
  (user/bind-key-global :basic :cut-word-right 'kill-word)
  ;; Set up basic copy/paste
  (user/bind-key-global :basic :selection-start 'set-mark-command)
  (user/bind-key-global :basic :copy 'kill-ring-save)
  (user/bind-key-global :basic :cut 'kill-region)
  (user/bind-key-global :basic :cut-expr 'kill-sexp)
  (user/bind-key-global :basic :paste 'yank)
  (user/bind-key-global :basic :cycle-paste 'yank-pop)

  ;; Rebind to new clipboard functions when available.
  (when (fboundp 'clipboard-kill-region)
    (global-set-key [remap kill-region] 'clipboard-kill-region))
  (when (fboundp 'clipboard-kill-ring-save)
    (global-set-key [remap kill-ring-save] 'clipboard-kill-ring-save))
  (when (fboundp 'clipboard-yank)
    (global-set-key [remap yank] 'clipboard-yank))

  ;;; (Packages) ;;;
  (req-package expand-region
    :config (user--expand-region-config))
  (req-package multiple-cursors
    :config (user--multiple-cursors-config))
  (req-package rect-mark))

(user--kill-yank-config)


(provide 'ux/kill-yank)
;;; kill-yank.el ends here
