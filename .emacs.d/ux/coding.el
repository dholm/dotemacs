;;; coding.el --- Coding system configuration
;;; Commentary:
;;; Code:

(defun user/coding-init ()
  "Initialize coding system."
  ;; Prefer UTF-8 if there is a choice
  (prefer-coding-system 'utf-8)
  ;; Set coding systems to UTF-8
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


(user/coding-init)


(provide 'ux/coding)
;;; coding.el ends here
