;;; html.el --- initializes HTML modes
;;; Commentary:
;;; Code:

(defun dholm/html-mode-hook ()
  "HTML mode hook.")

(defun guess-xhtml-hook ()
  "Guess whether the current buffer is XHTML."
  (when
      (save-excursion
        (search-forward-regexp "<[?]xml\\|//W3C//DTD XHTML" 80 t))
    (html-mode)))

(defun dholm/html-mode-init ()
  "Initialize HTML mode."
  ;; Configure mmm for HTML
  (after-load 'mmm
    (mmm-add-group
     'fancy-html
     '((html-php-tagged
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
    (add-auto-mode 'html-mode
                   "\\.inc$" "\\.php[34]?$" "\\.[sj]?html?$")

    ;; Enable support for mmm extensions
    (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js))
    (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil embedded-css))
    (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-html)))

  (add-hook 'html-mode-hook 'guess-xhtml-hook t)
  (add-hook 'html-mode-hook 'dholm/html-mode-hook))

(dholm/html-mode-init)


(require-package '(:name tidy))


(provide 'modes/html)
;;; html.el ends here
