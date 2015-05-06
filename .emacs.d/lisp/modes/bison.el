;;; bison --- Initializes Bison mode
;;; Commentary:
;;; Code:

(defun user/bison-mode-hook ()
  "Bison mode hook.")


(defun user/bison-mode-init ()
  "Initialize Bison mode."
  (add-hook 'bison-mode-hook 'user/bison-mode-hook))

(require-package '(:name bison-mode :after (user/bison-mode-init)))


(provide 'modes/bison)
;;; bison.el ends here
