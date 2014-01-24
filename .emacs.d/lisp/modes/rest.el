;;; rest --- reStructuredText support
;;; Commentary:
;;; Code:

(defun user/rst-mode-hook ()
  "Hook for reStructuredText mode."
  ;; Hook on save
  (add-hook 'write-contents-functions
            '(lambda ()
               ;; Update TOC
               (rst-toc-update)
               ;; Clean up adornments
               (rst-straighten-adornments)))

  ;; Enable auto completion
  (after-load 'auto-complete
    (auto-complete-rst-init)
    (auto-complete-mode t))

  ;;; (Bindings) ;;;
  ;; Adornments
  (define-key user/documentation-map (kbd "h") 'rst-display-adornments-hierarchy)
  (define-key user/code-map (kbd "h") 'rst-adjust)

  ;; Blocks
  (define-key user/code-map (kbd "l") 'rst-line-block-region)

  ;; Lists
  (define-key user/code-map (kbd "b") 'rst-bullet-list-region)
  (define-key user/code-map (kbd "e") 'rst-enumerate-region)

  ;; Table of Contents
  (define-key user/navigation-map (kbd "t") 'rst-toc)
  (define-key user/code-map (kbd "t") 'rst-toc-insert)

  ;; Compilation
  (define-key user/code-map (kbd "c") 'rst-compile))


(defun user/rst-mode-init ()
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
  (add-auto-mode 'rst-mode "\\.rst$" "\\.rest$")

  ;; Update TOC automatically if section headers are adjusted
  (add-hook 'rst-adjust-hook 'rst-toc-update)

  ;; Register mode hook
  (add-hook 'rst-mode-hook 'user/rst-mode-hook))

(require-package '(:name rst-mode :after (user/rst-mode-init)))
(require-package '(:name auto-complete-rst
                         :prepare (progn
                                    (autoload 'auto-complete-rst-init "auto-complete-rst"))))


(provide 'modes/rest)
;;; rest.el ends here
