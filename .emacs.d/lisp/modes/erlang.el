;;; erlang.el --- Erlang mode support
;;; Commentary:
;;; Code:

(defun user/erlang-mode-hook ()
  "Erlang mode hook."
  (user/gnu-global-enable)

  (with-feature 'edts-mode
    ;; Enable Erlang Development Tool Suite.
    (edts-mode t))

  (with-feature 'wrangler
    ;; Enable wrangler refactoring tool.
    (erlang-wrangler-on))

  ;; Bring in CEDET.
  (user/cedet-hook))


(defun user/erlang-mode-init ()
  "Initialize Erlang mode."
  (add-hook 'erlang-mode-hook 'user/erlang-mode-hook)

  (after-load 'erlang-mode
    (with-feature 'edts
      (require 'edts-start))

    (with-feature 'distel
      (distel-setup))))

(with-executable 'erl
  (require-package '(:name erlang-mode :after (user/erlang-mode-init)))
  (require-package '(:name edts))
  (require-package '(:name distel))
  (require-package '(:name wrangler)))


(provide 'modes/erlang)
;;; erlang.el ends here
