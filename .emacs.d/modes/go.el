;;; go.el --- Go mode support
;;; Commentary:
;;; Code:

(defun user/go-mode-hook ()
  "Go mode hook."
  ;; Automatic code fixes before saving
  (add-hook 'write-contents-functions
            '(lambda ()
               ;; Fix formatting
               (gofmt-before-save)))

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.go"))

  ;;; (Bindings) ;;;
  (define-key user/documentation-map (kbd "d") 'godef-describe)
  (define-key user/documentation-map (kbd "m") 'godoc)
  (define-key user/navigation-map (kbd "j") 'godef-jump)
  (define-key user/navigation-map (kbd "i") 'go-goto-imports))


(defun user/go-mode-init ()
  "Initialize Go mode."
  (require-package '(:name go-autocomplete))

  (add-hook 'go-mode-hook 'user/go-mode-hook))

(when *has-go*
  (require-package '(:name go-mode :after (user/go-mode-init))))


(provide 'modes/go)
;;; go.el ends here
