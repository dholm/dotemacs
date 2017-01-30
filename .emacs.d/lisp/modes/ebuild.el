;;; ebuild.el --- Gentoo ebuild mode support.
;;; Commentary:
;;; Code:

(defun user--ebuild-mode-hook ()
  "Gentoo ebuild mode hook.")


(defun user--ebuild-mode-config ()
  "Initialize Gentoo ebuild mode."
  (require-package '(:name ebuild-mode))

  (add-hook 'ebuild-mode-hook 'user--ebuild-mode-hook))

(user--ebuild-mode-config)


(provide 'modes/ebuild)
;;; ebuild.el ends here
