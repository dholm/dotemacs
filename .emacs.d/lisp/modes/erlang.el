;;; erlang.el --- Erlang mode support
;;; Commentary:
;;; Code:

(defun user--erlang-mode-hook ()
  "Erlang mode hook."
  (user/gnu-global-enable)

  (with-feature 'rainbow-delimiters
    ;; Enable rainbow delimiters.
    (rainbow-delimiters-mode t))

  (with-feature 'distel
    ;; Enable erlang node integration.
    (erlang-extended-mode t))

  (with-feature 'edts-mode
    ;; Enable Erlang Development Tool Suite.
    (edts-mode t))

  (with-feature 'wrangler
    ;; Enable wrangler refactoring tool.
    (erlang-wrangler-on))

  ;; Enable YouCompleteMe.
  (user/ycmd-enable)

  ;; Bring in CEDET.
  (user--cedet-hook))


(defun user--elixir-mode-hook ()
  "Erlang Elixir mode hook."
  (user--erlang-mode-hook)

  (with-feature 'alchemist
    (alchemist-mode t)))


(defun user--erlang-mode-config ()
  "Initialize Erlang mode."
  ;;; (Hooks) ;;;
  (add-hook 'erlang-mode-hook 'user--erlang-mode-hook)
  (add-hook 'elixir-mode-hook 'user--elixir-mode-hook)

  (after-load 'erlang-mode
    (with-feature 'edts
      (require 'edts-start))

    (with-feature 'distel
      (distel-setup))))

(with-executable 'erl
  (use-package erlang
    :defer t
    :after (user--erlang-mode-config))
  (use-package edts
    :defer t)
  (require-package '(:name distel))
  (require-package '(:name wrangler))
  (use-package alchemist
    :defer t))

(provide 'modes/erlang)
;;; erlang.el ends here
