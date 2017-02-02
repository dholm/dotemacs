;;; comint.el --- Interaction with inferior interpreters
;;; Commentary:
;;; Code:

(defun user--comint-mode-hook ()
  "Comint mode hook.")


(defun user--comint-config ()
  "Initialize comint mode."
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

  (add-hook 'comint-mode-hook 'user--comint-mode-hook)

  ;;; (Bindings) ;;;
  ;; Cycling through command history.
  (define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)
  (define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
  ;; Skip past prompt.
  (define-key comint-mode-map [C-left] 'comint-bol))

(after-load 'comint
  (user--comint-config))


(provide 'modes/comint)
;;; comint.el ends here
