;;; prog.el --- setup shared defaults for programming modes
;;; Commentary:
;;; Code:

(defvar fix-tabs-on-save nil
  "Automatically (un)tabify buffer on save according to 'indent-tabs-mode'.")


(defun user/prog-mode-hook ()
  "Programming mode hook."
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

  ;;; (Bindings) ;;;
  (define-key user/code-map (kbd "a") 'align-entire)
  (define-key user/code-map (kbd "-") 'comment-dwim))


(defun user/prog-mode-buffer-cleanup ()
  "Cleans up the buffer contents."
  (interactive)
  (delete-trailing-whitespace)
  (when fix-tabs-on-save
    (if indent-tabs-mode
        (tabify (point-min) (point-max))
      (untabify (point-min) (point-max))))
  ;; Return nil so that buffer is saved.
  nil)


(defun user/rainbow-delimiters-init ()
  "Initialize rainbow delimiters."
  (global-rainbow-delimiters-mode t))


(defun user/mic-paren-init ()
  "Initialize mic-paren."
  (paren-activate))


(defun user/prog-mode-init ()
  "Initialize generic programming mode."
  ;; Show matching parenthesis.
  (show-paren-mode t)

  (setq-default
   ;; Indent using spaces by default
   indent-tabs-mode nil
   ;; When using fill-paragraph or auto-fill-mode break lines at 80 characters by
   ;; default.
   fill-column 80)

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
