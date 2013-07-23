;;; visual-regexp --- visualize regular expressions
;;; Commentary:
;;; Code:

(require-package '(:name visual-regexp))


;;; (Bindings) ;;;
(global-set-key [remap query-replace-regexp] 'vr/query-replace)
(global-set-key [remap replace-regexp] 'vr/replace)


(provide 'ux/visual-regexp)
;;; visual-regexp.el ends here
