;;; kill-yank.el --- Configuration for copying and pasting of data
;;; Commentary:
;;; Code:

(defun user/expand-region-init ()
  "Initialize expand region."
  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :selection-expand 'er/expand-region))


(defun user/multiple-cursors-init ()
  "Initialize multiple cursors."
  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :selection-next 'mc/mark-next-like-this)
  (user/bind-key-global :basic :selection-prev 'mc/mark-previous-like-this)
  (user/bind-key-global :basic :selection-all 'mc/mark-all-like-this)
  (user/bind-key-global :basic :selection-edit-lines 'mc/edit-lines))


(defun user/kill-yank-init ()
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

  ;;; (Bindings) ;;;
  ;; Delete words with C/M-w and rebind kill/yank region to C-x C-k/C-x C-w.
  (user/bind-key-global :basic :cut-word-left 'backward-kill-word)
  (user/bind-key-global :basic :cut-word-right 'kill-word)
  ;; Set up basic copy/paste
  (user/bind-key-global :basic :selection-start 'set-mark-command)
  (user/bind-key-global :basic :copy 'kill-ring-save)
  (user/bind-key-global :basic :cut 'kill-region)
  (user/bind-key-global :basic :paste 'yank)
  (user/bind-key-global :basic :cycle-paste 'yank-pop)

  ;; Rebind to new clipboard functions when available.
  (when (fboundp 'clipboard-kill-region)
    (global-set-key [remap kill-region] 'clipboard-kill-region))
  (when (fboundp 'clipboard-kill-ring-save)
    (global-set-key [remap kill-ring-save] 'clipboard-kill-ring-save))
  (when (fboundp 'clipboard-yank)
    (global-set-key [remap yank] 'clipboard-yank))

  ;; Put backspace on C-h like in terminal.
  (global-set-key (kbd "C-h") 'delete-backward-char)

  ;;; (Packages) ;;;
  (require-package '(:name expand-region :after (user/expand-region-init)))
  (require-package '(:name multiple-cursors :after (user/multiple-cursors-init)))
  (require-package '(:name rect-mark)))

(user/kill-yank-init)


(provide 'ux/kill-yank)
;;; kill-yank.el ends here
