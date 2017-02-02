;;; smartparens.el --- Set up smartparens.
;;; Commentary:
;;; Code:

(defun user--smartparens-mode-hook ()
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
  (user/bind-key-local :basic :cut-expr 'sp-kill-sexp)
  (user/bind-key-local :basic :copy-expr 'sp-copy-sexp))


(defun user/smartparens-enable ()
  "Enable smartparens in current mode."
  (when (feature-p 'smartparens)
    (show-smartparens-mode t)
    (smartparens-mode t)))


(defun user--smartparens-config ()
  "Initialize smartparens."
  (validate-setq
   ;; Even kill whitespace with `sp-hybrid-kill'.
   sp-hybrid-kill-excessive-whitespace t)

  ;;; (Hooks) ;;;
  (add-hook 'smartparens-mode-hook 'user--smartparens-mode-hook))

(use-package smartparens
  :ensure t
  :bind (:map smartparens-mode-map
         ([remap kill-line] . sp-kill-hybrid-sexp))
  :config (user--smartparens-config))


(provide 'utilities/smartparens)
;;; smartparens.el ends here
