;;; racket.el --- Set up Racket mode
;;; Commentary:
;;; Code:

(defun user--racket-mode-hook ()
  "Racket mode hook."
  (enable-paredit-mode)

  (with-feature 'geiser-mode
    (setq
     geiser-scheme-implementation 'racket)

    (geiser-mode t))

  ;;; (Bindings) ;;;
  (user/bind-key-global :nav :follow-symbol 'racket-visit-definition)
  (user/bind-key-global :nav :go-back 'racket-unvisit)

  (user/bind-key-local :doc :reference 'racket-doc)
  (user/bind-key-local :doc :describe-function 'racket-describe)
  (user/bind-key-local :doc :describe-variable 'racket-describe)

  (unless (memq 'geiser-mode minor-mode-list)
    (user/bind-key-local :code :run 'racket-run))
  (user/bind-key-local :code :test 'racket-test))


(defun user--racket-repl-mode-hook ()
  "Racket REPL mode hook."
  (enable-paredit-mode)
  (rainbow-delimiters-mode t))


(defun user--racket-mode-config ()
  "Initialize Racket mode."
  ;;; (Hooks) ;;;
  (add-hook 'racket-mode-hook 'user--racket-mode-hook)
  (add-hook 'racket-repl-mode-hook 'user--racket-repl-mode-hook))

(with-executable 'racket
  (use-package racket-mode
    :defer t
    :config (user--racket-mode-config)))


(provide 'modes/racket)
;;; racket.el ends here
