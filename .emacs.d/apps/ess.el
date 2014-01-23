;;; ess.el --- Mathematical statistics support in Emacs
;;; Commentary:
;;; Code:

(defun user/ess-mode-common-hook ()
  "ESS common mode hook."
  (setq
   ;; Make R prompt read only.
   comint-prompt-read-only t)

  (when (el-get-package-is-installed 'ac-R)
    (ess-ac-init))

  ;;; (Bindings) ;;;
  (local-set-key [(shift return)] 'user/ess-eval))


(defun user/ess-mode-hook ()
  "ESS mode hook."
  (user/ess-mode-common-hook))


(defun user/Rnw-mode-hook ()
  "Rnw mode hook."
  (user/ess-mode-common-hook))


(defun user/ess-R-post-run-hook ()
  "ESS R post run hook."
  ;; Make R expand to the full width of the buffer.
  (ess-execute-screen-options))


(defun user/ac-R-init ()
  "Initialize R auto completion."
  (autoload 'ess-ac-init "ac-R"))


(defun user/ess-init ()
  "Initialize Emacs Speaks Statistics."
  (setq-default
   ;; Use auto completion in ESS modes.
   ess-use-auto-complete t
   ;; Use ElDoc in all ESS modes.
   ess-use-eldoc t
   ess-eldoc-show-on-symbol t
   ;; Optimize by only printing results, not code.
   ess-eval-visibly-p nil
   ;; Automatically scroll when output reaches bottom of buffer.
   ess-comint-scroll-to-bottom-on-output t
   ;; Start R in the current directory.
   ess-ask-for-ess-directory nil
   ess-local-process-name "R")

  (add-hook 'ess-mode-hook 'user/ess-mode-hook)
  (add-hook 'Rnw-mode-hook 'user/Rnw-mode-hook)
  (add-hook 'ess-R-post-run-hook 'user/ess-R-post-run-hook)

  ;;; (Bindings) ;;;
  (define-key user/utilities-map (kbd "s") 'R)

  ;;; (Functions) ;;;
  (defun user/ess-start-R ()
    "If R is not running launch it in an inferior frame."
    (interactive)
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
        (progn
          (delete-other-windows)
          (setq
           window1 (selected-window)
           window1-name (buffer-name)
          window2 (split-window window1))
          (R)
          (set-window-buffer window2 "*R*")
          (set-window-buffer window1 window1-name))))

  (defun user/ess-eval ()
    "Evaluate active region."
    (interactive)
    (user/ess-start-R)
    (if (and transient-mark-mode mark-active)
        (call-interactively 'ess-eval-line-and-step))))

(when *has-R*
  (require-package '(:name ess :after (user/ess-init)))
  (require-package '(:name ess-smart-underscore))
  (require-package '(:name ac-R :after (user/ac-R-init))))


(provide 'apps/ess)
;;; ess.el ends here
