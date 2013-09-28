;;; prog.el --- setup shared defaults for programming modes
;;; Commentary:
;;; Code:

(defun user/prog-mode-hook ()
  "Programming mode hook."
  (setq
   ;; Indent using spaces by default
   indent-tabs-mode nil
   ;; When using fill-paragraph or auto-fill-mode break lines at 80 characters by
   ;; default.
   fill-column 80)
  ;; Enable smart tabs
  (after-load 'tabkey2
    (tabkey2-mode t))
  ;; Automatically break long lines
  (auto-fill-mode t)
  (after-load 'diminish
    (diminish 'auto-fill-function))
  ;; Run spell-checker
  (flyspell-prog-mode)
  ;; Delete trailing whitespace on save
  (add-hook 'write-contents-functions 'user/prog-mode-buffer-cleanup nil t)
  ;; Enable dtrt-indent to attempt to identify the indentation rules used
  (after-load 'dtrt-indent
    (dtrt-indent-mode t))
  ;; Highlight FIXME/TODO/etc
  (after-load 'fix-mode
    (fic-mode t))
  ;; Diminish abbrev mode when loaded
  (after-load 'diminish
    (diminish 'abbrev-mode))
  ;; Automatically indent on newlines
  (local-set-key (kbd "RET") 'newline-and-indent))


(defun user/prog-mode-buffer-cleanup ()
  "Cleans up the buffer contents."
  (interactive)
  (delete-trailing-whitespace)
  (if indent-tabs-mode
      (tabify (point-min) (point-max))
    (untabify (point-min) (point-max)))
  ;; Return nil so that buffer is saved.
  nil)


(defun user/rainbow-delimiters-init ()
  "Initialize rainbow delimiters."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(rainbow-delimiters-depth-1-face ((t (:foreground ,cyan))))
         '(rainbow-delimiters-depth-2-face ((t (:foreground ,yellow))))
         '(rainbow-delimiters-depth-3-face ((t (:foreground ,blue))))
         '(rainbow-delimiters-depth-4-face ((t (:foreground ,orange))))
         '(rainbow-delimiters-depth-5-face ((t (:foreground ,green))))
         '(rainbow-delimiters-depth-6-face ((t (:foreground ,yellow))))
         '(rainbow-delimiters-depth-7-face ((t (:foreground ,blue))))
         '(rainbow-delimiters-depth-8-face ((t (:foreground ,orange))))
         '(rainbow-delimiters-depth-9-face ((t (:foreground ,green))))
         '(rainbow-delimiters-depth-10-face ((t (:foreground ,yellow))))
         '(rainbow-delimiters-depth-11-face ((t (:foreground ,blue))))
         '(rainbow-delimiters-depth-12-face ((t (:foreground ,orange))))
         '(rainbow-delimiters-unmatched-face
           ((t (:foreground ,solarized-fg :background ,solarized-bg
                            :inverse-video t))))))))

  (global-rainbow-delimiters-mode t))


(defun user/mic-paren-init ()
  "Initialize mic-paren."
  (paren-activate))


(defun user/prog-mode-init ()
  "Initialize generic programming mode."
  ;; Show matching parenthesis.
  (show-paren-mode t)

  (add-hook 'prog-mode-hook 'user/prog-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name fic-mode))
  (require-package '(:name mic-paren
                           :type emacswiki
                           :website "https://raw.github.com/emacsmirror/emacswiki.org/master/mic-paren.el"
                           :features (mic-paren)
                           :after (user/mic-paren-init)))
  (require-package '(:name rainbow-delimiters :after (user/rainbow-delimiters-init))))


(user/prog-mode-init)


(provide 'modes/prog)
;;; prog.el ends here
