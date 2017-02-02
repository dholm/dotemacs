;;; compile.el --- sets up Emacs compile support
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
     ((fboundp 'mode-compile) (call-interactively 'mode-compile))
     (t (call-interactively 'compile)))))


(defun user--mode-compile-config ()
  "Initialize mode-compile."
  (validate-setq
   ;; Set a sane compilation frame name.
   mode-compile-other-frame-name "*compilation*"
   ;; Run make with low priority and use multiple processes.
   mode-compile-make-program "nice make"
   mode-compile-default-make-options "-k -j"
   ;; Save the current buffer on compilation.
   mode-compile-always-save-buffer-p t)

  (after-load 'mode-compile
    (with-executable 'clang
      (add-to-list 'cc-compilers-list "clang")
      (add-to-list 'c++-compilers-list "clang++"))))


(defun user--compile-config ()
  "Initialize compile module."
  (validate-setq
   ;; Prevent input in compilation buffer.
   compilation-disable-input nil
   ;; Automatically scroll output.
   compilation-scroll-output t)

  (after-load 'popwin
    (add-to-list
     'popwin:special-display-config
     ;; Don't select compilation window when shown
     '(compilation-mode :height 20 :dedicated t)))

  ;;; (Hooks) ;;;
  (add-hook 'compilation-mode-hook 'user--compilation-mode-hook)
  (add-hook 'compilation-filter-hook 'user--compilation-filter-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :code :compile 'user/compile))

(use-package compile
  :defer t
  :config (user--compile-config))

(use-package mode-compile
  :defer t
  :config
  ;; Ensure byte-run has been loaded or mode-compile will override
  ;; `define-obsolete-variable-alias'.
  (when (load "byte-run.el" nil :noerror)
    (user--mode-compile-config)))

(provide 'utilities/compile)
;;; compile.el ends here
