;;; ruby --- ruby mode support
;;; Commentary:
;;; Code:

(defun dholm/ruby-mode-hook ()
  "Ruby mode hook."
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook))
  ;; Enable robe mode
  (robe-mode t)
  ;; Enable inferior ruby mode
  (inf-ruby-minor-mode t)
  ;; Enable yard and eldoc mode
  (yard-mode t)
  (eldoc-mode t)
  ;; Separate camel-case into separate words
  (subword-mode t)
  ;; Bindings
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map (kbd "RET") 'indent-for-tab-command))


(defun dholm/robe-mode-hook ()
  "Robe mode hook."
  (add-to-list 'ac-sources 'ac-source-robe)
  (set-auto-complete-as-completion-at-point-function))


(defun dholm/ruby-mode-init ()
  "Initialize Ruby mode."
  (add-hook 'ruby-mode-hook 'dholm/ruby-mode-hook))

(defun dholm/robe-init ()
  "Initialize robe."
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'dholm/robe-mode-hook))

(require-package '(:name ruby-mode :after (dholm/ruby-mode-init)))
(require-package '(:name robe-mode :after (dholm/robe-init)))
(require-package '(:name yard-mode
                         :type github
                         :pkgname "pd/yard-mode"))
(require-package '(:name inf-ruby))


(provide 'modes/ruby)
;;; ruby.el ends here
