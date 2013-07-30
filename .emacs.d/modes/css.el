;;; css --- initializes CSS modes
;;; Commentary:
;;; Code:

;; Set up HTML embedding support
(after-load 'mmm
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
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))


;; CSS editing
(defun dholm/css-mode-hook ()
  "CSS mode hook."
  (skewer-css-mode t)
  (rainbow-mode t)
  (ac-css-mode-setup)
  (setq
   cssm-indent-function #'cssm-c-style-indenter
   cssm-indent-level '4)
  (set (make-local-variable 'ac-auto-start) 2)
  (set (make-local-variable 'ac-auto-show-menu) t))

(add-hook 'css-mode-hook 'dholm/css-mode-hook)

(require-package '(:name skewer-mode))
(require-package '(:name showcss-mode))


(provide 'modes/css)
;;; css.el ends here
