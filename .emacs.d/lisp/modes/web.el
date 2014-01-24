;;; web.el --- Web development
;;; Commentary:
;;; Code:

(defun user/web-mode-hook ()
  "Web mode hook."
  (local-set-key (kbd "RET") 'newline-and-indent))


(defun user/tern-mode-hook ()
  "Tern mode hook."
  (define-key user/navigation-map (kbd "j") 'tern-find-definition)
  (define-key user/navigation-map (kbd "b") 'tern-pop-find-definition)
  (define-key user/documentation-map (kbd "d") 'tern-get-docs)
  (tern-ac-setup))


(defun user/web-mode-init ()
  "Initialize web mode."
  (setq-default
   ;; Indent HTML automatically.
   web-mode-indent-style 2
   ;; Indentation offsets.
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   ;; Highlight current HTML element.
   web-mode-enable-current-element-highlight t)

  (add-hook 'web-mode-hook 'user/web-mode-hook)

  (add-auto-mode 'web-mode "\\.html?$" "\\.phtml$" "\\.php[34]?$")

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(web-mode-builtin-face ((t (:foreground ,red))))
         '(web-mode-comment-face ((t (:foreground ,base01))))
         '(web-mode-constant-face ((t (:foreground ,blue :weight bold))))
         '(web-mode-current-element-highlight-face ((t (:underline unspecified :weight unspecified
                                                                   :background ,base02))))
         '(web-mode-css-at-rule-face ((t (:foreground ,violet :slant italic))))
         '(web-mode-css-pseudo-class-face ((t (:foreground ,green :slant italic))))
         '(web-mode-doctype-face ((t (:foreground ,base01 :slant italic :weight bold))))
         '(web-mode-folded-face ((t (:underline t))))
         '(web-mode-function-name-face ((t (:foreground ,blue))))
         '(web-mode-html-attr-name-face ((t (:foreground ,blue :slant normal))))
         '(web-mode-html-attr-value-face ((t (:foreground ,cyan :slant italic))))
         '(web-mode-html-tag-face ((t (:foreground ,green))))
         '(web-mode-keyword-face ((t (:foreground ,yellow :weight normal))))
         '(web-mode-preprocessor-face ((t (:foreground ,yellow :slant normal :weight unspecified))))
         '(web-mode-string-face ((t (:foreground ,cyan))))
         '(web-mode-type-face ((t (:foreground ,yellow))))
         '(web-mode-variable-name-face ((t (:foreground ,blue))))
         '(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
         '(web-mode-block-attr-name-face ((t (:inherit web-mode-html-attr-name-face))))
         '(web-mode-block-attr-value-face ((t (:inherit web-mode-html-attr-value-face))))
         '(web-mode-block-comment-face ((t (:inherit web-mode-comment-face))))
         '(web-mode-block-control-face ((t (:inherit font-lock-preprocessor-face))))
         '(web-mode-block-face ((t (:background unspecified))))
         '(web-mode-block-string-face ((t (:inherit web-mode-string-face))))
         '(web-mode-comment-keyword-face ((t (:box 1 :weight bold))))
         '(web-mode-css-color-face ((t (:inherit font-lock-builtin-face))))
         '(web-mode-css-function-face ((t (:inherit font-lock-builtin-face))))
         '(web-mode-css-priority-face ((t (:inherit font-lock-builtin-face))))
         '(web-mode-css-property-name-face ((t (:inherit font-lock-variable-name-face))))
         '(web-mode-css-selector-face ((t (:inherit font-lock-keyword-face))))
         '(web-mode-css-string-face ((t (:inherit web-mode-string-face))))
         '(web-mode-javascript-string-face ((t (:inherit web-mode-string-face))))
         '(web-mode-json-context-face ((t (:foreground ,violet))))
         '(web-mode-json-key-face ((t (:foreground ,violet))))
         '(web-mode-json-string-face ((t (:inherit web-mode-string-face))))
         '(web-mode-param-name-face ((t (:foreground ,base0))))
         '(web-mode-part-comment-face ((t (:inherit web-mode-comment-face))))
         '(web-mode-part-face ((t (:inherit web-mode-block-face))))
         '(web-mode-part-string-face ((t (:inherit web-mode-string-face))))
         '(web-mode-symbol-face ((t (:foreground ,yellow))))
         '(web-mode-whitespace-face ((t (:background ,red))))
         '(web-mode-html-tag-bracket-face ((t (:foreground ,base01)))))))))


(defun user/tern-init ()
  "Initialize tern."
  (add-hook 'tern-mode-hook 'user/tern-mode-hook))


(defun user/web-init ()
  "Initialize web development."
  (require-package '(:name web-mode :after (user/web-mode-init)))
  (require-package '(:name tern :after (user/tern-init)))
  (require-package '(:name skewer-mode))
  (require-package '(:name tidy)))

(user/web-init)


(provide 'modes/web)
;;; web.el ends here
