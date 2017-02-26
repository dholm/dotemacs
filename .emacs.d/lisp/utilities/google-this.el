;;; google-this.el --- Google item under point
;;; Commentary:
;;; Code:

(use-package google-this
  :defer
  :diminish google-this-mode
  :init
  (user/bind-key-global :util :google 'google-search)
  (user/bind-key-global :util :google-at-point 'google-this)
  (user/bind-key-global :util :google-selection 'google-region))


(provide 'utilities/google-this)
;;; google-this.el ends here
