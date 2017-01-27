;;; ebuild.el --- Gentoo ebuild mode support.
;;; Commentary:
;;; Code:

(defun user/ebuild-mode-hook ()
  "Gentoo ebuild mode hook.")


(defun user/ebuild-mode-init ()
  "Initialize Gentoo ebuild mode."
  (req-package ebuild-mode
    :loader :el-get)

  (add-hook 'ebuild-mode-hook 'user/ebuild-mode-hook))

(user/ebuild-mode-init)


(provide 'modes/ebuild)
;;; ebuild.el ends here
