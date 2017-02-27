;;; vlf --- Initializes view large file mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vlf
  :defer
  :init
  (user/bind-key-global :basic :view-file 'vlf))


(provide 'modes/vlf)
;;; vlf.el ends here
