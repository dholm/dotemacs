;;; doc-view.el --- Emacs document viewer
;;; Commentary:
;;; Code:

(defun user/doc-view-mode-hook ()
  "Document viewer mode hook."
  ;;; (Bindings) ;;;
  (when (feature-p 'doc-view-fit-to-page)
    (local-set-key "f" 'doc-view-fit-page-to-window)
    (local-set-key "w" 'doc-view-fit-width-to-window)
    (local-set-key "h" 'doc-view-fit-height-to-window))

  (user/bind-key-local :nav :go-forward 'doc-view-scroll-up-or-next-page)
  (user/bind-key-local :nav :go-back 'doc-view-scroll-down-or-previous-page))


(defun user/doc-view-init ()
  "Initialize document viewer."
  (setq-default
   ;; Render documents in high resolution.
   doc-view-resolution 300)

  (let ((dvi "\\.dvi$")
        (msoffice "\\.(doc|xls|ppt)x?$")
        (opendoc "\\.(od[tspgcim]|ot[tspg])$")
        (ps "\\.e?ps$")
        (pdf "\\.pdf$")
        (patterns))
    (with-executable 'pdftotext
      (add-to-list 'patterns pdf)
      (with-executable 'ps2pdf
        (add-to-list 'patterns ps)))

    (with-executable 'gs
      (add-to-list 'patterns ps)
      (when (display-graphic-p)
        (add-to-list 'patterns pdf)))

    (with-any-executable '(dvipdf dvipdfm)
      (add-to-list 'patterns dvi))

    (with-executable 'unoconv
      (add-to-list 'patterns opendoc)
      (add-to-list 'patterns msoffice))

    ;; Register doc-view-mode for patterns.
    (mapc (lambda (pattern)
            (add-auto-mode 'doc-view-mode-maybe pattern)) patterns))

  (add-hook 'doc-view-mode-hook 'user/doc-view-mode-hook)

  ;;; (Packages) ;;;
  (req-package doc-present
    :loader :el-get)
  (when (display-graphic-p)
    (req-package doc-view-fit-to-page
      :loader :el-get)))

(user/doc-view-init)


(provide 'modes/doc-view)
;;; doc-view.el ends here
