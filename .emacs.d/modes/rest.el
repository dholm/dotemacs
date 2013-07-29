;;; rest --- reStructuredText support
;;; Commentary:
;;; Code:

(defun dholm/rst-mode-hook ()
  "Hook for reStructuredText mode."
  (setq-default
   ;; Indent using spaces
   indent-tabs-mode nil)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode)
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t)
  ;; Enable auto completion
  (after-load 'auto-complete
    (auto-complete-rst-init)
    (auto-complete-mode t)))


(defun dholm/rst-mode-init ()
  "Initialize reStructuredText mode."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(rst-level-1-face ((t (:background ,yellow  :foreground ,base03))))
         '(rst-level-2-face ((t (:background ,cyan    :foreground ,base03))))
         '(rst-level-3-face ((t (:background ,blue    :foreground ,base03))))
         '(rst-level-4-face ((t (:background ,violet  :foreground ,base03))))
         '(rst-level-5-face ((t (:background ,magenta :foreground ,base03))))
         '(rst-level-6-face ((t (:background ,red     :foreground ,base03))))))))

  ;; Register auto modes
  (add-auto-mode 'rst-mode
                 "\\.txt$" "\\.text$" "\\.rst$" "\\.rest$")
  ;; Register mode hook
  (add-hook 'rst-mode-hook 'dholm/rst-mode-hook))

(require-package '(:name rst-mode :after (dholm/rst-mode-init)))
(require-package '(:name auto-complete-rst
                         :prepare (progn
                                    (autoload 'auto-complete-rst-init "auto-complete-rst"))))

(provide 'modes/rest)
;;; rest.el ends here
