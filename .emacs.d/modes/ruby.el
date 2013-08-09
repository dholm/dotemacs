;;; ruby --- ruby mode support
;;; Commentary:
;;; Code:

(defconst *has-ruby* (executable-find "ruby"))


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
  ;; Bindings
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map (kbd "RET") 'indent-for-tab-command))


(defun user/robe-mode-hook ()
  "Robe mode hook."
  (add-to-list 'ac-sources 'ac-source-robe)
  (set-auto-complete-as-completion-at-point-function))


(defun user/ruby-mode-init ()
  "Initialize Ruby mode."
  (add-hook 'ruby-mode-hook 'user/ruby-mode-hook))

(defun user/robe-init ()
  "Initialize robe."
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'user/robe-mode-hook))


(when *has-ruby*
  (require-package '(:name ruby-mode :after (user/ruby-mode-init)))
  (require-package '(:name robe-mode :after (user/robe-init)))
  (require-package '(:name inf-ruby)))


(provide 'modes/ruby)
;;; ruby.el ends here
