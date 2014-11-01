;;; text.el --- text mode support
;;; Commentary:
;;; Code:

(defun user/text-mode-hook ()
  "Text mode hook."
  (user/fundamental-mode-hook)

  (setq
   ;; Colons are followed by two spaces.
   colon-double-space t
   ;; Sentences end after two spaces.
   sentence-end-double-space t
   ;; When using fill-paragraph or auto-fill-mode break lines at 79 characters
   ;; by default.
   fill-column 79
   ;; Indent using spaces.
   indent-tabs-mode nil)

  ;; Run spell-checker.
  (flyspell-mode t)

  (with-feature 'pandoc-mode
    (pandoc-mode t))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :fill-paragraph 'fill-paragraph))


(defun user/text-mode-init ()
  "Initialize generic text editing mode."
  (add-hook 'text-mode-hook 'user/text-mode-hook))

(user/text-mode-init)


(provide 'modes/text)
;;; text.el ends here
