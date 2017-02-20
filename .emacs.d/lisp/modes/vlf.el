;;; vlf --- Initializes view large file mode
;;; Commentary:
;;; Code:

(use-package vlf
  :defer t
  :init
  (user/bind-key-global :basic :view-file 'vlf))


(provide 'modes/vlf)
;;; vlf.el ends here
