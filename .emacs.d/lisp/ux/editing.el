;;; editing.el --- Configure Emacs editing
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


(defun user/editing-init ()
  "Initialize editing in Emacs."
  (when (eq window-system 'ns)
    (setq
     ;; Swap command and option on MacOS X.
     mac-option-modifier 'alt
     mac-command-modifier 'meta
     ;; Use right option key for writing special characters.
     mac-right-option-modifier nil))

  ;;; (Bindings) ;;;
  (global-set-key [remap move-beginning-of-line] 'beginning-or-indentation)
  (global-set-key [remap move-end-of-line] 'end-of-line-or-code)
  (user/bind-key-global :code :join-line 'join-line)
  (user/bind-key-global :code :fill-paragraph 'fill-paragraph))

(user/editing-init)


(provide 'ux/editing)
;;; editing.el ends here
