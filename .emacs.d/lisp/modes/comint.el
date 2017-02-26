;;; comint.el --- Interaction with inferior interpreters
;;; Commentary:
;;; Code:

(use-package comint
  :ensure nil
  :defer
  :config
  (validate-setq
   ;; Scroll automatically on new output.
   comint-scroll-to-bottom-on-output 'others
   comint-scroll-show-maximum-output t
   comint-move-point-for-output 'others
   ;; Scroll buffer to bottom in active frame on input.
   comint-scroll-to-bottom-on-input 'this
   ;; Make the prompt read only.
   comint-prompt-read-only t
   ;; Set a decent input history size.
   comint-input-ring-size 10000
   ;; Don't store duplicates in history.
   comint-input-ignoredups t)

  ;;; (Bindings) ;;;
  ;; Cycling through command history.
  (define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)
  (define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
  ;; Skip past prompt.
  (define-key comint-mode-map [C-left] 'comint-bol))


(provide 'modes/comint)
;;; comint.el ends here
