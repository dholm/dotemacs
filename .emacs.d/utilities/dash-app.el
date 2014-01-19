;;; dash-app.el --- Dash documentation browser
;;; Commentary:
;;; Code:

(defun user/dash-at-point-init ()
  "Initialize Dash-at-point."
  ;;; (Bindings) ;;;
  (define-key user/documentation-map (kbd "r") 'dash-at-point))


(defun user/dash-app-init ()
  "Initialize Dash support for Emacs."
  (require-package '(:name dash-at-point :after (user/dash-at-point-init))))

(when (and (eq system-type 'darwin)
         (file-directory-p "/Applications/Dash.app"))
  (user/dash-app-init))


(provide 'utilities/dash-app)
;;; dash-app.el ends here
