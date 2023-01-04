;;; compile.el --- sets up Emacs compile support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--compilation-mode-hook ()
  "Compilation mode hook.")


(defun user--compilation-filter-hook ()
  "Hook for filtering compilation output."
  ;; Temporarily make buffer writable.
  (let ((inhibit-read-only t))
    ;; Colorize compilation output.
    (ansi-color-apply-on-region (point-min) (point-max))))


(defun user/compile ()
  "Compile current context."
  (interactive)
  (let ((ede-proj (user/proj-from-path user/ede-proj (path-abs-buffer))))
    (cond
     (ede-proj (user/proj-build ede-proj))
     ((feature-p 'flex-compile) (call-interactively 'flex-compile-compile))
     (t (call-interactively 'compile)))))

(use-package compile
  :defer
  :init
  (user/bind-key-global :code :compile 'user/compile)
  :hook ((compilation-mode-hook . user--compilation-mode-hook)
         (compilation-filter-hook . user--compilation-filter-hook))
  :config
  (validate-setq
   ;; Prevent input in compilation buffer.
   compilation-disable-input nil
   ;; Automatically scroll output.
   compilation-scroll-output t)

  (with-eval-after-load 'popwin
    (add-to-list
     'popwin:special-display-config
     ;; Don't select compilation window when shown
     '(compilation-mode :height 20 :dedicated t)))

  (use-package fancy-compilation
    :commands (fancy-compilation-mode)
    :init
    (with-eval-after-load 'compile
      (fancy-compilation-mode)))

  (use-package flex-compile
    :disabled
    :pin "MELPA"))


(provide 'utilities/compile)
;;; compile.el ends here
