;;; (Initialization) ;;;
(require-package '(:name minimap))


;;; (Functions) ;;;
(defun minimap-toggle ()
  "Toggle minimap for current buffer."
  (interactive)
  (if (null minimap-bufname)
    (minimap-create)
    (minimap-kill)))


;;; (Bindings) ;;;
(when (display-graphic-p)
  (global-set-key [f2] 'minimap-toggle))


(provide 'utilities/minimap)
