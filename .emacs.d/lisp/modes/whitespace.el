;;; whitespace.el --- whitespace mode support
;;; Commentary:
;;; Code:

(defvar user/whitespace-mode-suppressed nil
  "Tracks whether `whitespace-mode' is currently being suppressed.")
(make-variable-buffer-local 'user/prev-whitespace-mode-suppressed)


(defun user/whitespace-mode-suppress (suppress)
  "If SUPPRESS is non-nil, disable `whitespace-mode' in current mode."
  (when (boundp 'whitespace-mode)
    (if suppress
        (when (and whitespace-mode (not user/whitespace-mode-suppressed))
          (setq user/whitespace-mode-suppressed t)
          (whitespace-mode -1))
      (when user/whitespace-mode-suppressed
        (setq user/whitespace-mode-suppressed nil)
        (whitespace-mode 1)))))


(defun user/whitespace-disable-style (styles)
  "Disable STYLES in current mode."
  (when (boundp 'whitespace-style)
    (let ((options (user/filter (lambda (x) (member x styles))
                                whitespace-style)))
      (when (and (boundp 'global-whitespace-mode) global-whitespace-mode)
        (global-whitespace-toggle-options options))
      (when (and (boundp 'whitespace-mode) whitespace-mode)
        (whitespace-toggle-options options)))))


(defun user/whitespace-init ()
  "Initialize whitespace mode."
  (setq-default
   ;; Maximum allowed line length.
   whitespace-line-column 120
   ;; Cleanup whitespace errors on save.
   whitespace-action '(auto-cleanup)
   ;; Kinds of whitespace to visualize.
   whitespace-style
   '(;; Visualize using faces.
     face
     ;; Mark any tabs.
     tab-mark
     ;; Empty lines at beginning or end of buffer.
     empty
     ;; Trailing whitespace.
     trailing
     ;; Lines that extend beyond `whitespace-line-column.'
     lines
     ;; Wrong kind of indentation (tab when spaces and vice versa.)
     indentation
     ;; Mixture of space and tab on the same line.
     space-before-tab space-after-tab))

  (when (eq default-terminal-coding-system 'utf-8)
    (setq-default
     ;; Special characters for displaying whitespace.
     whitespace-display-mappings
     '(;; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
       (space-mark 32 [183] [46])
       ;; 10 LINE FEED, 9166 RETURN SYMBOL 「⏎」, 36 DOLLAR SIGN 「$」
       (newline-mark 10 [9166 10] [36 10])
       ;; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」, 92 9 CHARACTER TABULATION 「\t」
       (tab-mark 9 [9655 9] [92 9]))))

  (after-load 'whitespace
    (after-load 'diminish
      (diminish 'whitespace-mode))))

(user/whitespace-init)


(provide 'modes/whitespace)
;;; whitespace.el ends here
