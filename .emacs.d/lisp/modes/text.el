;;; text.el --- text mode support
;;; Commentary:
;;; Code:

(defun user--text-mode-hook ()
  "Text mode hook."
  (user--fundamental-mode-hook)

  (user/smartparens-enable)

  (setq
   ;; Colons are followed by two spaces.
   colon-double-space t
   ;; Sentences end after two spaces.
   sentence-end-double-space t
   ;; When using fill-paragraph or auto-fill-mode break lines at 79 characters
   ;; by default.
   fill-column 79
   ;; Indent using spaces.
   indent-tabs-mode nil)

  ;; Protect against missing dictionary.
  (try-eval
      ;; Run spell-checker in programming mode.
      (flyspell-mode t))

  (with-feature 'pandoc-mode
    (pandoc-mode t))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :fill-paragraph 'fill-paragraph))

(use-package text-mode
  :ensure nil
  :defer t
  :init
  (add-hook 'text-mode-hook 'user--text-mode-hook)
  :config
  (after-load 'smartparens
    (sp-with-modes '(text-mode)
      (sp-local-pair "`" "'" :actions '(insert wrap)))))


(provide 'modes/text)
;;; text.el ends here
