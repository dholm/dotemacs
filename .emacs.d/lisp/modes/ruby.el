;;; ruby.el --- ruby mode support
;;; Commentary:
;;; Code:

(defun user/ruby-mode-hook ()
  "Ruby mode hook."
  ;; Bring in CEDET.
  (user/cedet-hook)

  ;; Enable robe mode
  (robe-mode t)

  ;; Enable inferior ruby mode
  (inf-ruby-minor-mode t)

  ;; Enable eldoc mode
  (eldoc-mode t)

  ;; Separate camel-case into separate words
  (subword-mode t)

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.rb"))

  ;;; (Bindings) ;;;
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)
  (with-feature 'yari
    (user/bind-key-local :doc :reference 'yari)))


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

(with-executable 'ruby
  (user/ruby-mode-init))


(provide 'modes/ruby)
;;; ruby.el ends here
