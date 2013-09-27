;;; yasnippet.el --- Emacs snippet support
;;; Commentary:
;;; Code:

(defun user/yasnippet-init ()
  "Initialize yasnippet."
  (setq-default
   yas-snippet-dirs (list
                     (path-join user-emacs-directory "snippets")
                     (path-join *user-data-directory* "snippets"))
   yas-prompt-functions (list
                         'yas-completing-prompt
                         'yas-dropdown-prompt
                         'yas-ido-prompt)
   yas-indent-line 'auto)

  (when (eq window-system 'x)
    (add-to-list 'yas-prompt-functions 'yas-x-prompt))

  (yas--initialize)

  (after-load 'ux/completion
    (add-ac-sources 'ac-source-yasnippet))

  ;;; (Functions) ;;;
  (defun user/yas-goto-end-of-active-field ()
    "Move cursor to the end of the active yasnippet field."
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-end (yas--snippet-active-field
                                      snippet))))
      (if (= (point) position)
          (move-end-of-line)
        (goto-char position))))

  (defun user/yas-goto-start-of-active-field ()
    "Move cursor to the beginning of the active yasnippet field."
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-start (yas--snippet-active-field
                                        snippet))))
      (if (= (point) position)
          (move-beginning-of-line)
        (goto-char position))))

  ;;; (Bindings) ;;;
  (define-key yas-keymap (kbd "RET") 'yas-exit-all-snippets)
  (define-key yas-keymap (kbd "C-e") 'user/yas-goto-end-of-active-field)
  (define-key yas-keymap (kbd "C-a") 'user/yas-goto-start-of-active-field))


(defun user/helm-c-yasnippet-init ()
  "Initialize Helm for Yasnippet."
  (setq-default
   helm-c-yas-space-match-any-greedy t)

  (define-key user/code-map (kbd "y") 'helm-c-yas-complete))


(require-package '(:name yasnippet :after (user/yasnippet-init)))
(require-package '(:name helm-c-yasnippet :after (user/helm-c-yasnippet-init)))


(provide 'utilities/yasnippet)
;;; yasnippet.el ends here
