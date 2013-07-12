;;; (Initialization) ;;;
(require-package (:name ace-jump-mode))


;;; (Bindings) ;;;
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


(provide 'ux/ace-jump-mode)
