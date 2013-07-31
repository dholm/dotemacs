;;; rest --- reStructuredText support
;;; Commentary:
;;; Code:

(defun dholm/rst-mode-hook ()
  "Hook for reStructuredText mode."
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
         '(rst-level-1-face ((t (:foreground ,solarized-bg :background ,yellow))))
         '(rst-level-2-face ((t (:foreground ,solarized-bg :background ,cyan))))
         '(rst-level-3-face ((t (:foreground ,solarized-bg :background ,blue))))
         '(rst-level-4-face ((t (:foreground ,solarized-bg :background ,violet))))
         '(rst-level-5-face ((t (:foreground ,solarized-bg :background ,magenta))))
         '(rst-level-6-face ((t (:foreground ,solarized-bg :background ,red))))))))

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
