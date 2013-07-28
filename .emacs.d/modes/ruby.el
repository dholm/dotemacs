;;; ruby --- ruby mode support
;;; Commentary:
;;; Code:

(defun dholm/ruby-mode-hook ()
  "Ruby mode hook."
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook))
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode t)
  ;; Enable robe mode
  (robe-mode t)
  ;; Enable inferior ruby mode
  (inf-ruby-minor-mode t)
  ;; Enable yard and eldoc mode
  (yard-mode t)
  (eldoc-mode t)
  ;; Separate camel-case into separate words
  (subword-mode t)
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t)
  ;; Bindings
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map (kbd "RET") 'indent-for-tab-command))


(defun dholm/robe-mode-hook ()
  "Robe mode hook."
  (add-to-list 'ac-sources 'ac-source-robe)
  (set-auto-complete-as-completion-at-point-function))


(add-hook 'ruby-mode-hook 'dholm/ruby-mode-hook)
(add-hook 'robe-mode-hook 'dholm/robe-mode-hook)


(defun dholm/robe-init ()
  "Initialize robe."
  (add-hook 'ruby-mode-hook 'robe-mode))

(require-package '(:name ruby-mode))
(require-package '(:name robe-mode :after (dholm/robe-init)))
(require-package '(:name yard-mode))
(require-package '(:name inf-ruby))


(provide 'modes/ruby)
;;; ruby.el ends here
