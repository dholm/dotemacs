;;; pdf.el --- Emacs PDF modes
;;; Commentary:
;;; Code:

(defun user--pdf-view-mode-hook ()
  "PDF view mode hook."
  (user/bind-key-local :nav :goto-line 'pdf-view-goto-page))


(defun user--pdf-tools-config ()
  "Initialize PDF tools."
  (setq-default
   ;; Fit page to view by default.
   pdf-view-display-size 'fit-page)

  ;; Register auto modes.
  (add-auto-mode 'pdf-view-mode "\\.pdf$")

  ;;; (Hooks) ;;;
  (add-hook 'pdf-view-mode-hook 'user--pdf-view-mode-hook))

(when (and (display-graphic-p)
           (pkg-config-has-p "poppler-glib"))
  (use-package pdf-tools
    :defer t
    :config (user--pdf-tools-config)))


(provide 'modes/pdf)
;;; pdf.el ends here
