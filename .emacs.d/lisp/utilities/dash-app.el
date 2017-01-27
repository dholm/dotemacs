;;; dash-app.el --- Dash documentation browser
;;; Commentary:
;;; Code:

(defun user--dash-at-point-config ()
  "Initialize Dash-at-point."
  ;;; (Bindings) ;;;
  (user/bind-key-global :doc :reference 'dash-at-point))


(defun user--dash-app-config ()
  "Initialize Dash support for Emacs."
  (req-package dash-at-point
    :config (user--dash-at-point-config)))

(when (osx-app-installed-p "com.kapeli.dash")
  (user--dash-app-config))


(provide 'utilities/dash-app)
;;; dash-app.el ends here
