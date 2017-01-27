;;; google-this.el --- Google item under point
;;; Commentary:
;;; Code:

(defun user--google-this-config ()
  "Initialize Google this."
  (google-this-mode t)
  (after-load 'diminish
    (diminish 'google-this-mode))

  ;;; (Bindings) ;;;
  (user/bind-key-global :util :google 'google-search)
  (user/bind-key-global :util :google-at-point 'google-this)
  (user/bind-key-global :util :google-selection 'google-region))

(req-package google-this
  :config (user--google-this-config))


(provide 'utilities/google-this)
;;; google-this.el ends here
