;;; minimap --- a code outline when in graphical mode
;;; Commentary:
;;; Code:

(defun dholm/minimap-init ()
  "Initialize minimap."
  ;;; (Functions) ;;;
  (defun minimap-toggle ()
    "Toggle minimap for current buffer."
    (interactive)
    (if (null minimap-bufname)
        (minimap-create)
      (minimap-kill)))

  ;;; (Bindings) ;;;
  (define-key dholm/utilities-map (kbd "m") 'minimap-toggle))

(when (display-graphic-p)
  (require-package '(:name minimap :after (dholm/minimap-init))))


(provide 'utilities/minimap)
;;; minimap.el ends here
