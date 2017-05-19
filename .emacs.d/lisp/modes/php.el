;;; php.el --- initializes PHP modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--php-mode-hook ()
  "PHP mode hook."
  ;; Bring in CEDET.
  (with-feature 'cedet
    (user--cedet-hook)
    (when (derived-mode-p 'web-mode)
      ;; In web-mode CEDET doesn't automatically load PHP support.
      (wisent-php-default-setup)))

  (user/gnu-global-enable)
  (user/smartparens-enable)

  ;; Separate camel-case into separate words
  (subword-mode t))

(use-package php-mode
  :if (executable-find "php")
  :defer
  :init
  (with-eval-after-load 'web-mode
    (user--add-web-mode-hook 'php 'user--php-mode-hook))
  :config
  (with-eval-after-load 'smartparens
    (defun user/php-handle-docstring (&rest _ignored)
      (-when-let (line (save-excursion
                         (forward-line)
                         (thing-at-point 'line)))
        (cond
         ((string-match-p "function" line)
          (save-excursion
            (insert "\n")
            (let ((args (save-excursion
                          (forward-line)
                          (my-php-get-function-args))))
              (--each args
                (insert (format "* @param %s\n" it)))))
          (insert "* "))
         ((string-match-p ".*class\\|interface" line)
          (save-excursion (insert "\n*\n* @author\n"))
          (insert "* ")))
        (let ((o (sp--get-active-overlay)))
          (indent-region (overlay-start o) (overlay-end o)))))

    (sp-with-modes '(php-mode)
      (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                                 (user/php-handle-docstring "RET")))
      (sp-local-pair "/*." ".*/" :post-handlers '(("| " "SPC")))
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
      (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))))


(provide 'modes/php)
;;; php.el ends here
