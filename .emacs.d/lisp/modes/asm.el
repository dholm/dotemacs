;;; asm.el --- Initializes assembly mode
;;; Commentary:
;;; Code:

(defun user/asm-mode-hook ()
  "Assembly mode hook."
  (setq
   ;; Indent using tabs by default.
   indent-tabs-mode t)

  (user/gnu-global-enable))


(define-derived-mode arm-mode asm-mode "ARM"
  "Major mode for editing ARM assembler code."
  ;; Unset ; key.
  (local-unset-key (vector asm-comment-char))
  (set (make-local-variable 'asm-comment-char) ?@)
  (local-set-key (vector asm-comment-char) 'asm-comment)
  ;; Update syntax for new comment char.
  (set-syntax-table (make-syntax-table asm-mode-syntax-table))
  (modify-syntax-entry asm-comment-char "< b")
  ;; Fix one level comments.
  (set (make-local-variable 'comment-start) (string asm-comment-char)))


(define-derived-mode mips-mode asm-mode "MIPS"
  "Major mode for editing MIPS assembler code."
  ;; Unset ; key.
  (local-unset-key (vector asm-comment-char))
  (set (make-local-variable 'asm-comment-char) ?#)
  (local-set-key (vector asm-comment-char) 'asm-comment)
  ;; Update syntax for new comment char.
  (set-syntax-table (make-syntax-table asm-mode-syntax-table))
  (modify-syntax-entry asm-comment-char "< b")
  ;; Fix one level comments.
  (set (make-local-variable 'comment-start) (string asm-comment-char)))


(defun user/iasm-mode-init ()
  "Initialize interactive assmelby mode."
  ;;; (Bindings) ;;;
  (user/bind-key-global :code :library-list 'iasm-ldd)
  (user/bind-key-global :code :disassemble 'iasm-disasm))


(defun user/asm-mode-init ()
  "Initialize assembly mode."
  (add-hook 'asm-mode-hook 'user/asm-mode-hook)
  (add-auto-mode 'asm-mode "\\.[ia]65$")

  ;;; (Packages) ;;;
  (require-package '(:name iasm-mode :after (user/iasm-mode-init))))

(user/asm-mode-init)


(provide 'modes/asm)
;;; asm.el ends here
