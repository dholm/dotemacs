;;; prog.el --- setup shared defaults for programming modes
;;; Commentary:
;;; Code:

(defun user/prog-mode-hook ()
  "Programming mode hook."
  (user/fundamental-mode-hook)

  ;; Run spell-checker in programming mode.
  (flyspell-prog-mode)

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :comment 'comment-dwim))


(defun user/prog-mode-init ()
  "Initialize generic programming mode."
  (add-hook 'prog-mode-hook 'user/prog-mode-hook))

(user/prog-mode-init)


(provide 'modes/prog)
;;; prog.el ends here
