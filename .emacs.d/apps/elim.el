;;; elim.el --- instant messenger
;;; Commentary:
;;; Code:

(defun user/elim-init ()
  "Initialize elim."
  (setq-default
   lui-max-buffer-size 30000
   lui-flyspell-p t
   lui-flyspell-alist '(("." "american"))
   elim-directory (path-join *user-cache-directory* "elim"))

  (define-key user/utilities-map (kbd "i") 'garak))


(when *has-libpurple*
  (require-package '(:name elim :after (user/elim-init))))


(provide 'apps/elim)
;;; elim.el ends here
