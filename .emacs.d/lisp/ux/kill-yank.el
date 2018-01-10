;;; kill-yank.el --- Configuration for copying and pasting of data -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package mouse
  :ensure nil
  :config
  (validate-setq
   ;; Mouse selection should not automatically go to kill ring.
   mouse-drag-copy-region nil)
  (when (eq window-system 'x)
    (validate-setq
     ;; Don't inject mouse selection into X11 clipboard.
     mouse-drag-copy-region nil)

    ;; Set middle mouse button to paste from primary X11 selection.
    (global-set-key [mouse-2] 'mouse-yank-primary)))

(use-package select
  :if window-system
  :ensure nil
  :config
  (validate-setq
   ;; Do not interact with X11 primary selection.
   select-enable-primary nil
   ;; Make kill/yank interact with X11 clipboard selection.
   select-enable-clipboard t
   ;; Active region should set primary X11 selection.
   select-active-regions t))

(use-package menu-bar
  :ensure nil
  :config
  ;; Rebind to new clipboard functions when available.
  (when (fboundp 'clipboard-kill-region)
    (global-set-key [remap kill-region] 'clipboard-kill-region))
  (when (fboundp 'clipboard-kill-ring-save)
    (global-set-key [remap kill-ring-save] 'clipboard-kill-ring-save))
  (when (fboundp 'clipboard-yank)
    (global-set-key [remap yank] 'clipboard-yank)))

(use-package expand-region
  :defer
  :init
  (user/bind-key-global :basic :selection-expand 'er/expand-region))

(use-package multiple-cursors
  :defer
  :init
  (user/bind-key-global :basic :selection-next 'mc/mark-next-like-this)
  (user/bind-key-global :basic :selection-prev 'mc/mark-previous-like-this)
  (user/bind-key-global :basic :selection-all 'mc/mark-all-like-this)
  (user/bind-key-global :basic :selection-edit-lines 'mc/edit-lines))

(use-package rect-mark
  :defer
  :quelpa (rect-mark
           :fetcher url
           :url "https://www.emacswiki.org/emacs/download/rect-mark.el"))

(use-package simple
  :ensure nil
  :init
  ;; Delete words with C/M-w and rebind kill/yank region to C-x C-k/C-x C-w.
  (user/bind-key-global :basic :cut-word-left 'backward-kill-word)
  (user/bind-key-global :basic :cut-word-right 'kill-word)
  ;; Set up basic copy/paste
  (user/bind-key-global :basic :selection-start 'set-mark-command)
  (user/bind-key-global :basic :copy 'kill-ring-save)
  (user/bind-key-global :basic :cut 'kill-region)
  (user/bind-key-global :basic :paste 'yank)
  (user/bind-key-global :basic :cycle-paste 'yank-pop)
  :config
  (validate-setq
   ;; Increase the maximum number of saved kill ring entries.
   kill-ring-max 200
   ;; Do not store duplicates in kill ring.
   kill-do-not-save-duplicates t
   ;; Save clipboard before killing it.
   save-interprogram-paste-before-kill t))

(use-package move-text
  :defer)

(user/bind-key-global :basic :cut-expr 'kill-sexp)


(provide 'ux/kill-yank)
;;; kill-yank.el ends here
