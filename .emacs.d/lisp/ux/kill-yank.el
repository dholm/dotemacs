;;; kill-yank.el --- Configuration for copying and pasting of data
;;; Commentary:
;;; Code:

(defun user/expand-region-init ()
  "Initialize expand region."
  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :selection-expand 'er/expand-region))


(defun user/kill-yank-init ()
  "Initialize copy/paste."
  (setq-default
   ;; Mouse selection should not automatically go to kill ring.
   mouse-drag-copy-region nil)

  (when (eq window-system 'x)
    (setq-default
     ;; Do not interact with X11 primary selection.
     x-select-enable-primary nil
     ;; Interact with X11 clipboard selection.
     x-select-enable-clipboard t
     ;; Yanking should also use the clipboard.
     interprogram-paste-function 'x-cut-buffer-or-selection-value))

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

  ;; Put backspace on C-h like in terminal.
  (global-set-key (kbd "C-h") 'delete-backward-char)

  ;;; (Packages) ;;;
  (require-package '(:name expand-region :after (user/expand-region-init))))

(user/kill-yank-init)


(provide 'ux/kill-yank)
;;; kill-yank.el ends here
