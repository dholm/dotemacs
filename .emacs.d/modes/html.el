;;; (Initialization) ;;;

;; Set up mmm group for HTML editing
(mmm-add-group
 'fancy-html
 '(
   (html-php-tagged
    :submode php-mode
    :face mmm-code-submode-face
    :front "<[?]php"
    :back "[?]>")
   (html-css-embedded
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "\]*>"
    :back "")
   (html-css-attribute
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "\\bstyle=\\s-*\""
    :back "\"")
   (html-javascript-attribute
    :submode javascript-generic-mode
    :face mmm-code-submode-face
    :front "\\bon\\w+=\\s-*\""
    :back "\"")))

;; Register file extensions for html-mode
(add-to-list 'auto-mode-alist '("\\.inc\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.[sj]?html?\\'" . html-mode))

;; Enable support for mmm extensions
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil embedded-css))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-html))


;; Support for detecting XHTML
(defun guess-xhtml-hook ()
  "Guess whether the current buffer is XHTML."
  (when
      (save-excursion
        (search-forward-regexp "<[?]xml\\|//W3C//DTD XHTML" 80 t))
    (html-mode)))
(add-hook 'html-mode-hook 'guess-xhtml-hook t)


(defun dholm/html-mode-hook ()
  ;; Configure nxml auto completion
  (setq nxml-slash-auto-complete-flag t))

(add-hook 'html-mode-hook 'dholm/html-mode-hook)


(provide 'modes/html)
