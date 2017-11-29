;;; prog.el --- setup shared defaults for programming modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--prog-mode-hook ()
  "Programming mode hook."
  (user--fundamental-mode-hook)

  (when (user-flyspell-p)
    ;; Protect against missing dictionary.
    (try-eval
        ;; Run spell-checker in programming mode.
        (flyspell-prog-mode)))

  ;; Buttonize links.
  (goto-address-prog-mode t)

  (outline-minor-mode t)

  ;; Try to enable completion system.
  (cond
   ((user/auto-complete-p) (auto-complete-mode t))
   ((user/company-mode-p) (company-mode t)))

  (when (feature-p 'comment-tags)
    (comment-tags-mode t))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :comment (if (feature-p 'comment-dwim-2)
                                          'comment-dwim-2
                                        'comment-dwim))
  (with-feature 'quickrun
    (user/bind-key-local :code :eval-buffer 'quickrun)
    (user/bind-key-local :code :eval-selection 'quickrun-region)))


(use-package prog-mode
  :ensure nil
  :init
  (add-hook 'prog-mode-hook 'user--prog-mode-hook)
  :config
  ;;; (Packages) ;;;
  (use-package subword
    :ensure nil
    :diminish subword-mode)
  (use-package comment-dwim-2)
  (use-package quickrun)
  (use-package comment-tags
    :diminish comment-tags-mode
    :config
    (setq
     comment-tags/keymap-prefix (user/get-key :nav :find-todos))))


(provide 'modes/prog)
;;; prog.el ends here
