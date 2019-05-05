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
  :bind-wrap
  (((:key :basic :selection-expand) . er/expand-region)
   ((:key :basic :select-paragraph) . er/mark-paragraph)
   ((:key :basic :select-function) . er/mark-defun)
   ((:key :basic :select-inside) . er/mark-inside-pairs)))

(use-package multiple-cursors
  :defer
  :bind-wrap
  (((:key :basic :selection-next) . mc/mark-next-like-this)
   ((:key :basic :selection-prev) . mc/mark-previous-like-this)
   ((:key :basic :selection-all) . mc/mark-all-like-this)
   ((:key :basic :selection-edit-lines) . mc/edit-lines)))

(use-package rect-mark
  :defer
  :quelpa (rect-mark
           :fetcher url
           :url "https://www.emacswiki.org/emacs/download/rect-mark.el"))

(use-package simple
  :ensure nil
  :bind-wrap
  (;; Delete words with C/M-w and rebind kill/yank region to C-x C-k/C-x C-w.
   ((:key :basic :cut-word-left) . backward-kill-word)
   ((:key :basic :cut-word-right) . kill-word)
   ;; Set up basic copy/paste
   ((:key :basic :selection-start) . set-mark-command)
   ((:key :basic :copy) . kill-ring-save)
   ((:key :basic :cut) . kill-region)
   ((:key :basic :paste) . yank)
   ((:key :basic :cycle-paste) . yank-pop))
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
