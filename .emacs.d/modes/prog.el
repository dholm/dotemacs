;;; prog --- setup shared defaults for programming modes
;;; Commentary:
;;; Code:

(defun user/prog-mode-hook ()
  "Programming mode hook."
  (setq-default
   ;; Indent using spaces by default
   indent-tabs-mode nil)
  ;; Automatically break long lines
  (auto-fill-mode t)
  (after-load 'diminish
    (diminish 'auto-fill-function))
  ;; Run spell-checker
  (flyspell-prog-mode)
  ;; Delete trailing whitespace on save
  (add-hook 'write-contents-functions 'delete-trailing-whitespace nil t)
  ;; Enable dtrt-indent to attempt to identify the indentation rules used
  (after-load 'dtrt-indent
    (dtrt-indent-mode t))
  ;; Highlight FIXME/TODO/etc
  (when (el-get-package-is-installed 'fic-mode)
    (fic-mode t))
  ;; Diminish abbrev mode when loaded
  (after-load 'diminish
    (diminish 'abbrev-mode)))

(add-hook 'prog-mode-hook 'user/prog-mode-hook)

(require-package '(:name fic-mode))


(provide 'modes/prog)
;;; prog.el ends here
