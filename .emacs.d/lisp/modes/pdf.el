;;; pdf.el --- Emacs PDF modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--pdf-view-mode-hook ()
  "PDF view mode hook."
  (user/bind-key-local :nav :goto-line 'pdf-view-goto-page))

(when (and (display-graphic-p)
           (pkg-config-has-p "poppler-glib"))
  (use-package pdf-tools
    :defer
    :mode ("\.pdf$" . pdf-view-mode)
    :init
    (add-hook 'pdf-view-mode-hook 'user--pdf-view-mode-hook)
    :config
    (validate-setq
     ;; Fit page to view by default.
     pdf-view-display-size 'fit-page)))


(provide 'modes/pdf)
;;; pdf.el ends here
