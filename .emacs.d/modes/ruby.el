;;; ruby.el --- ruby mode support
;;; Commentary:
;;; Code:

(defun user/ruby-mode-hook ()
  "Ruby mode hook."
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook))
  ;; Enable robe mode
  (robe-mode t)
  ;; Enable inferior ruby mode
  (inf-ruby-minor-mode t)
  ;; Enable eldoc mode
  (eldoc-mode t)
  ;; Separate camel-case into separate words
  (subword-mode t)

  ;;; (Bindings) ;;;
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)
  (when (el-get-package-is-installed 'yari)
    (define-key user/help-map (kbd "SPC") 'yari))

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.rb")))


(defun user/robe-mode-hook ()
  "Robe mode hook."
  (robe-ac-setup))


(defun user/robe-init ()
  "Initialize robe."
  (add-hook 'robe-mode-hook 'user/robe-mode-hook))


(defun user/ruby-mode-init ()
  "Initialize Ruby mode."
  (require-package '(:name ruby-mode))
  (require-package '(:name robe-mode :after (user/robe-init)))
  (require-package '(:name inf-ruby))
  (require-package '(:name yari))

  (add-hook 'ruby-mode-hook 'user/ruby-mode-hook))

(when *has-ruby*
  (user/ruby-mode-init))


(provide 'modes/ruby)
;;; ruby.el ends here
