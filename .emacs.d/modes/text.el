;;; text.el --- text mode support
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


(defun user/text-mode-init ()
  "Initialize generic text editing mode."
  (add-hook 'text-mode-hook 'user/text-mode-hook))

(user/text-mode-init)


(provide 'modes/text)
;;; text.el ends here
