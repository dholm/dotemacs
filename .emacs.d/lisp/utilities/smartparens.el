;;; smartparens.el --- Set up smartparens.
;;; Commentary:
;;; Code:

(defun user/smartparens-mode-hook ()
  "Mode hook for smartparens."
  ;;; (Bindings) ;;;
  ;; Marking.
  (user/bind-key-local :basic :selection-next 'sp-select-next-thing-exchange)
  (user/bind-key-local :basic :selection-prev 'sp-select-previous-thing)

  ;; Editing.
  (user/bind-key-local :basic :forward-word 'sp-forward-symbol)
  (user/bind-key-local :basic :backward-word 'sp-backward-symbol)
  (user/bind-key-local :basic :forward-expr 'sp-forward-sexp)
  (user/bind-key-local :basic :backward-expr 'sp-backward-sexp)
  (user/bind-key-local :code :unwrap-expr 'sp-unwrap-sexp)
  (user/bind-key-local :code :comment 'sp-comment)

  ;; Killing.
  (local-set-key [remap kill-line] 'sp-kill-hybrid-sexp)
  (user/bind-key-local :basic :cut-expr 'sp-kill-sexp)
  (user/bind-key-local :basic :copy-expr 'sp-copy-sexp))


(defun user/smartparens-enable ()
  "Enable smartparens in current mode."
  (when (feature-p 'smartparens)
    (show-smartparens-mode t)
    (smartparens-mode t)))


(defun user/smartparens-init ()
  "Initialize smartparens."
  (setq-default
   ;; Even kill whitespace with `sp-hybrid-kill'.
   sp-hybrid-kill-excessive-whitespace t)

  ;;; (Hooks) ;;;
  (add-hook 'smartparens-mode-hook 'user/smartparens-mode-hook))

(req-package smartparens
  :config (user/smartparens-init))


(provide 'utilities/smartparens)
;;; smartparens.el ends here
