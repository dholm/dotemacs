;;; go.el --- Go mode support
;;; Commentary:
;;; Code:

(defconst *has-go* (executable-find "go"))


(defun user/go-mode-hook ()
  "Go mode hook."
  ;; Automatic code fixes before saving
  (add-hook 'write-contents-functions
            '(lambda ()
               ;; Fix formatting
               (gofmt-before-save)))

  ;;; (Bindings) ;;;
  (define-key user/documentation-map (kbd "d") 'godef-describe)
  (define-key user/documentation-map (kbd "m") 'godoc)
  (define-key user/navigation-map (kbd "j") 'godef-jump)
  (define-key user/navigation-map (kbd "i") 'go-goto-imports))


(defun user/go-mode-init ()
  "Initialize Go mode."
  (add-hook 'go-mode-hook 'user/go-mode-hook))

(when *has-go*
  (require-package '(:name go-mode :after (user/go-mode-init)))
  (require-package '(:name go-autocomplete
                           :type github
                           :pkgname "nsf/gocode"
                           :depends (auto-complete)
                           :features (go-autocomplete)
                           :build '(("go" "build"))
                           :load-path ("emacs")
                           :prepare (progn
                                      (setq exec-path
                                            (append exec-path
                                                    (el-get-package-directory "gocode")))))))


(provide 'modes/go)
;;; go.el ends here
