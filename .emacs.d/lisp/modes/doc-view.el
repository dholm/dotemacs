;;; doc-view.el --- Emacs document viewer -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package doc-view
  :defer
  :bind-wrap
  (:map doc-view-mode-map
        ((:key :nav :go-forward) . doc-view-scroll-up-or-next-page)
        ((:key :nav :go-back) . doc-view-scroll-down-or-previous-page))
  :config
  (validate-setq
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

  (use-package doc-present
    :quelpa (doc-present
             :fetcher github
             :repo "dengste/doc-present")
    :init
    (autoload 'doc-present "doc-present" nil t)
    (autoload 'doc-present-mode "doc-present" nil t))

  (use-package doc-view-fit-to-page
    :disabled
    :if window-system
    :el-get t
    :bind
    (:map doc-view-mode-map
          ("f" . doc-view-fit-page-to-window)
          ("w" . doc-view-fit-width-to-window)
          ("h" . doc-view-fit-height-to-window))))


(provide 'modes/doc-view)
;;; doc-view.el ends here
