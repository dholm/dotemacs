;;; minimap.el --- a code outline when in graphical mode
;;; Commentary:
;;; Code:

(defun user/minimap-init ()
  "Initialize minimap."
  ;;; (Functions) ;;;
  (defun minimap-toggle ()
    "Toggle minimap for current buffer."
    (interactive)
    (if (null minimap-bufname)
        (minimap-create)
      (minimap-kill)))

  ;;; (Bindings) ;;;
  (define-key user/utilities-map (kbd "m") 'minimap-toggle))

(when (display-graphic-p)
  (require-package '(:name minimap :after (user/minimap-init))))


(provide 'utilities/minimap)
;;; minimap.el ends here
