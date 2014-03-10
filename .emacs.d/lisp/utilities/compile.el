;;; compile.el --- sets up Emacs compile support
;;; Commentary:
;;; Code:

(defun user/compilation-mode-hook ()
  "Compilation mode hook.")


(defun user/compile ()
  "Compile current context."
  (interactive)
  (let ((ede-proj (user/current-path-apply 'user/ede-project)))
    (cond
     (ede-proj (project-compile-project ede-proj
                                        (read-string "Build command: "
                                                     (oref ede-proj compile-command))))
     ((fboundp 'mode-compile) (call-interactively 'mode-compile))
     (t (call-interactively 'compile)))))


(defun user/mode-compile-init ()
  "Initialize mode-compile."
  (setq-default
   ;; Set a sane compilation frame name.
   mode-compile-other-frame-name "*compilation*"
   ;; Run make with low priority and use multiple processes.
   mode-compile-make-program "nice make"
   mode-compile-default-make-options "-k -j")

  (after-load 'mode-compile
    (with-executable 'clang
      (setq-defeault
       cc-compilers-list (cons "clang" cc-compilers-list)
       c++-compilers-list (cons "clang++" c++-compilers-list)))))


(defun user/compile-init ()
  "Initialize compile module."
  (setq-default
   ;; Prevent input in compilation buffer
   compilation-disable-input nil
   ;; Automatically scroll output
   compilation-scroll-output t
   ;; Save the current buffer on compilation
   mode-compile-always-save-buffer-p t)

  ;; Add compilation mode hook
  (add-hook 'compilation-mode-hook 'user/compilation-mode-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :code :compile 'user/compile)

  ;;; (Packages) ;;;
  (require-package '(:name mode-compile :after (user/mode-compile-init))))

(user/compile-init)


(provide 'utilities/compile)
;;; compile.el ends here
