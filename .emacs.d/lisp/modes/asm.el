;;; asm.el --- Initializes assembly mode
;;; Commentary:
;;; Code:

(defun user/asm-mode-hook ()
  "Assembly mode hook.")


(defun user/iasm-mode-init ()
  "Initialize interactive assmelby mode."
  ;;; (Bindings) ;;;
  (user/bind-key-global :code :library-list 'iasm-ldd)
  (user/bind-key-global :code :disassemble 'iasm-disasm))


(defun user/asm-mode-init ()
  "Initialize assembly mode."
  (add-hook 'asm-mode-hook 'user/asm-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name iasm-mode :after (user/iasm-mode-init))))

(user/asm-mode-init)


(provide 'modes/asm)
;;; asm.el ends here
