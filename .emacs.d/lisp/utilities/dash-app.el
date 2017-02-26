;;; dash-app.el --- Dash documentation browser
;;; Commentary:
;;; Code:

(when (osx-app-installed-p "com.kapeli.dash")
  (use-package dash-at-point
    :defer
    :init
    (user/bind-key-global :doc :reference 'dash-at-point)))


(provide 'utilities/dash-app)
;;; dash-app.el ends here
