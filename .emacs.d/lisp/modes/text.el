;;; text.el --- text mode support
;;; Commentary:
;;; Code:

(defun user/text-mode-hook ()
  "Text mode hook."
  (setq
   ;; Indent using spaces by default
   indent-tabs-mode nil
   ;; Colons are followed by two spaces
   colon-double-space t
   ;; Sentences end after two spaces
   sentence-end-double-space t
   ;; When using fill-paragraph or auto-fill-mode break lines at 79 characters
   ;; by default.
   fill-column 79)
  ;; Automatically break long lines
  (auto-fill-mode t)
  ;; Run spell-checker
  (flyspell-mode)
  ;; Delete trailing whitespace on save
  (add-hook 'write-contents-functions 'delete-trailing-whitespace nil t)
  ;; Enable dtrt-indent to attempt to identify the indentation rules used
  (after-load 'dtrt-indent
    (dtrt-indent-mode t))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :fill-paragraph 'fill-paragraph))


(defun user/text-mode-init ()
  "Initialize generic text editing mode."
  (add-hook 'text-mode-hook 'user/text-mode-hook))

(user/text-mode-init)


(provide 'modes/text)
;;; text.el ends here
