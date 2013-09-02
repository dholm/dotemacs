;;; init-modes.el --- initializes major modes
;;; Commentary:
;;; Code:

(defun user/utf8-init ()
  "Initialize UTF-8 support."
  ;; Prefer UTF-8
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(user/utf8-init)

;; Load modes
(load-all-files-from-dir (path-join user-emacs-directory "modes"))


(provide 'init-modes)
;;; init-modes.el ends here
