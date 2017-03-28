;;; editing.el --- Configure Emacs editing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun beginning-or-indentation ()
  "Move cursor to beginning of line or to its indentation."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))


(defun point-in-comment ()
  "Determine if the point is inside a comment."
  (interactive)
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
         (not (nth 3 syn)))))


(defun end-of-line-or-code (arg)
  "Move point to end of line or forward ARG.

If already there, move back to end of code.  By 'end of code' we
mean before a possible comment.  Comments are recognized in any
mode that sets `syntax-ppss' properly."
  (interactive "P")
  (let ((eol (save-excursion
               (move-end-of-line arg)
               (point))))
    (cond ((= (point) eol)
           (move-end-of-line arg)
           (while (point-in-comment)
             (backward-char))
           (skip-chars-backward " \t"))
          (t (move-end-of-line arg)))))

(defun user--editing-config ()
  "Initialize editing in Emacs."
  (validate-setq
   ;; Increase history.
   history-length 1000)

  ;; Enable narrowing.
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)

  ;; Enable case modification.
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;;; (Packages) ;;;
  (use-package simple
    :ensure nil
    :diminish auto-fill-function)

  (when (eq window-system 'ns)
    (use-package ns-win
      :ensure nil
      :config
      (validate-setq
       ;; Swap command and option on MacOS X.
       mac-option-modifier 'alt
       mac-command-modifier 'meta
       ;; Use right option key for writing special characters.
       mac-right-option-modifier nil)))

  (when (feature-p 'helm)
    (use-package helm-unicode
      :bind ("C-c h 8" . helm-unicode)))

  (use-package selected
    :ensure t
    :commands selected-minor-mode
    :init
    (setq
     selected-org-mode-map (make-sparse-keymap))
    (selected-global-mode t)
    :bind (:map selected-keymap
                ;; Region.
                ("q" . selected-off)
                ("e" . er/expand-region)
                ;; Modification.
                ("f" . fill-region)
                ("u" . upcase-region)
                ("d" . downcase-region)
                ("s" . sort-lines)
                ("m" . apply-macro-to-region-lines)
                ;; Information.
                ("w" . count-words-region)
                ;; Motion.
                ("p" . move-text-up)
                ("n" . move-text-down)
                :map selected-org-mode-map
                ("t" . org-table-convert-region)))

  ;;; (Bindings) ;;;
  (global-set-key [remap move-beginning-of-line] 'beginning-or-indentation)
  (global-set-key [remap move-end-of-line] 'end-of-line-or-code)
  (user/bind-key-global :basic :forward-word 'forward-word)
  (user/bind-key-global :basic :backward-word 'backward-word)
  (user/bind-key-global :basic :forward-expr 'forward-sexp)
  (user/bind-key-global :basic :backward-expr 'backward-sexp)
  (user/bind-key-global :basic :del-char-left 'delete-backward-char)
  (user/bind-key-global :basic :del-char-right 'delete-char)
  (user/bind-key-global :basic :widen 'widen)
  (user/bind-key-global :basic :narrow-to-page 'narrow-to-page)
  (user/bind-key-global :basic :narrow-to-region 'narrow-to-region)
  (user/bind-key-global :basic :narrow-to-function 'narrow-to-defun)
  (user/bind-key-global :code :join-line 'join-line)
  (user/bind-key-global :code :fill-paragraph 'fill-paragraph))

(user--editing-config)


(provide 'ux/editing)
;;; editing.el ends here
