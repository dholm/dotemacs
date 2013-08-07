;;; text --- text mode support
;;; Commentary:
;;; Code:

(defun user/text-mode-hook ()
  "Text mode hook."
  (setq-default
   ;; Indent using spaces by default
   indent-tabs-mode nil)
  ;; Automatically break long lines
  (auto-fill-mode t)
  ;; Run spell-checker
  (flyspell-mode)
  ;; Delete trailing whitespace on save
  (add-hook 'write-contents-functions 'delete-trailing-whitespace nil t)
  ;; Enable dtrt-indent to attempt to identify the indentation rules used
  (after-load 'dtrt-indent
    (dtrt-indent-mode t)))

(add-hook 'text-mode-hook 'user/text-mode-hook)


(provide 'modes/text)
;;; text.el ends here
