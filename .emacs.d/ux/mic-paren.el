;;; mix-paren --- show matching parenthesis even if outside current view
;;; Commentary:
;;; Code:

(defun dholm/mic-paren-init ()
  "Initialize mic-paren."
  (paren-activate))

(require-package '(:name mic-paren
			 :type emacswiki
			 :website "https://raw.github.com/emacsmirror/emacswiki.org/master/mic-paren.el"
			 :features (mic-paren)
			 :after (dholm/mic-paren-init)))


(provide 'ux/mic-paren)
;;; mic-paren.el ends here
