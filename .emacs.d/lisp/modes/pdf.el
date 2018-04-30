;;; pdf.el --- Emacs PDF modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--pdf-view-mode-hook ()
  "PDF view mode hook."
  (user/bind-key-local :nav :goto-line 'pdf-view-goto-page))

(use-package pdf-tools
  :if (pkg-config-has-p "poppler-glib")
  :config
  (use-package pdf-view
    :ensure nil
    :if window-system
    :hook (pdf-view-mode-hook . user--pdf-view-mode-hook)
    :mode ("\.pdf$" . pdf-view-mode)
    :config
    (validate-setq
     ;; Fit page to view by default.
     pdf-view-display-size 'fit-page
     pdf-view-use-imagemagick t
     pdf-view-midnight-colors '("white smoke" . "gray5"))

    (use-package org-pdfview))

  (pdf-tools-install))


(provide 'modes/pdf)
;;; pdf.el ends here
