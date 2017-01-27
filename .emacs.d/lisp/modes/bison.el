;;; bison --- Initializes Bison mode
;;; Commentary:
;;; Code:

(defun user--bison-mode-hook ()
  "Bison mode hook.")


(defun user--bison-mode-config ()
  "Initialize Bison mode."
  (add-hook 'bison-mode-hook 'user--bison-mode-hook))

(req-package bison-mode
  :config (user--bison-mode-config))


(provide 'modes/bison)
;;; bison.el ends here
