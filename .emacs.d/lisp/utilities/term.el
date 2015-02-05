;;; term.el --- Initialize the Emacs terminal
;;; Commentary:
;;; Code:

(defun user/term-mode-hook ()
  "Term mode hook."
  (user/shell-mode-common-hook))


(defun user/term-exec-hook ()
  "Term startup hook."
  ;; Automatically close buffer when finished.
  (let* ((buf (current-buffer))
         (proc (get-buffer-process buf)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buf))))))


(defun user/term-init ()
  "Initialize the Emacs terminal."
  ;;; (Hooks) ;;;
  (add-hook 'term-exec-hook 'user/term-exec-hook)
  (add-hook 'term-mode-hook 'user/term-mode-hook)

  ;;; (Bindings) ;;;
  (after-load 'term
    (define-key term-raw-map (kbd "C-c C-y") 'term-paste)))

(user/term-init)


(provide 'utilities/term)
;;; term.el ends here
