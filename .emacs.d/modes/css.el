;;; css --- initializes CSS modes
;;; Commentary:
;;; Code:

(require-package '(:name skewer-mode))
(require-package '(:name showcss-mode))


;; Set up HTML embedding support
(mmm-add-group
 'html-css
 '((css-cdata
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</script>" @)))
   (css
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t]*\n?"
    :back "[ \t]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</style>" @)))
   (css-inline
    :submode css-mode
    :face mmm-code-submode-face
    :front "style=\""
    :back "\"")))

(dolist (mode (list 'html-mode 'nxml-mode))
  (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))


;; CSS editing
(defun dholm/css-mode-hook ()
  (skewer-css-mode)
  (rainbow-mode)
  (ac-css-mode-setup)
  (setq
   cssm-indent-function #'cssm-c-style-indenter
   cssm-indent-level '4)
  (set (make-local-variable 'ac-auto-start) 2)
  (set (make-local-variable 'ac-auto-show-menu) t))

(add-hook 'css-mode-hook 'dholm/css-mode-hook)


(provide 'modes/css)
;;; css.el ends here
