;;; asm.el --- Initializes assembly mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--asm-mode-hook ()
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


(use-package asm-mode
  :defer
  :mode "\.[ia]65$"
  :init
  (add-hook 'asm-mode-hook 'user--asm-mode-hook)
  :config
  ;;; (Packages) ;;;
  (use-package iasm-mode
    :bind-wrap
    (:map asm-mode-map
          ((:key :code :library-list) . iasm-ldd)
          ((:key :code :disassemble) . iasm-disasm)
     :map c-mode-base-map
          ((:key :code :library-list) . iasm-disasm-link-buffer)
          ((:key :code :disassemble) . iasm-goto-disasm-buffer))))


(provide 'modes/asm)
;;; asm.el ends here
