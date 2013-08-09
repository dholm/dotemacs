;;; elim.el --- instant messenger
;;; Commentary:
;;; Code:

(defconst *has-purple*
  (and
   (pkg-config-has-p "libxml-2.0")
   (pkg-config-has-p "purple")))


(defun user/elim-init ()
  "Initialize elim."
  (setq-default
   lui-max-buffer-size 30000
   lui-flyspell-p t
   lui-flyspell-alist '(("." "american"))
   elim-directory (path-join *user-cache-directory* "elim"))

  (define-key user/utilities-map (kbd "i") 'garak))


(when *has-purple*
  (require-package '(:name elim :after (user/elim-init))))


(provide 'utilities/elim)
;;; elim.el ends here
