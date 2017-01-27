;;; cogre.el --- Initializes COGRE mode
;;; Commentary:
;;; Code:

(defun user--cogre-mode-hook ()
  "COGRE mode hook."
  (when (eq default-terminal-coding-system 'utf-8)
    (cogre-uml-enable-unicode)))


(defun user--cogre-mode-config ()
  "Initialize COGRE mode."
  ;;; (Hooks) ;;;
  (add-hook 'cogre-mode-hook 'user--cogre-mode-hook))

(user--cogre-mode-config)


(provide 'modes/cogre)
;;; cogre.el ends here
