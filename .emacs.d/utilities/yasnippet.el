;;; yasnippet.el --- Emacs snippet support
;;; Commentary:
;;; Code:

(defun user/yasnippet-init ()
  "Initialize yasnippet."
  (setq-default
   ;; Paths to where snippets will be stored.
   yas-snippet-dirs (list
                     (path-join user-emacs-directory "snippets")
                     (path-join *user-data-directory* "snippets"))
   ;; Setup up preferred completion functions.
   yas-prompt-functions '(yas-completing-prompt
                          yas-dropdown-prompt
                          yas-ido-prompt)
   ;; Only expand in specific contexts.
   yas-expand-only-for-last-commands '(self-insert-command
                                       yas-exit-all-snippets
                                       yas-abort-snippet
                                       yas-skip-and-clear-or-delete-char
                                       yas-next-field-or-maybe-expand)
   ;; Automatically indent lines.
   yas-indent-line 'auto
   ;; Wrap expansion around selected region.
   yas-wrap-around-region t)

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
