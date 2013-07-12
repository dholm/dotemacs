;;; (Initialization) ;;;
(require-package (:name visual-regexp))


;;; (Bindings) ;;;
(global-set-key [remap query-replace-regexp] 'vr/query-replace)
(global-set-key [remap replace-regexp] 'vr/replace)


(provide 'ux/visual-regexp)
