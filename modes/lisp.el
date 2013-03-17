;; (Code Conventions) ;;

;; (Utilities) ;;

;; newLISP utility files
(setq load-path (cons "~/.emacs.d/utilities/newlisp" load-path))
(require 'newlisp)
(when (featurep 'newlisp)
  (add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))
  (add-to-list 'interpreter-mode-alist '("newlisp" . newlisp-mode)))

(defun swank-newlisp-init (port-filename coding-system)
  (format "%S\n" `(swank:start-server ,port-filename)))

(defvar swank-newlisp-filename "swank-newlisp.lsp")
(defun slime-newlisp ()
  (interactive)
  (let ((slime-lisp-implementations
         `((newlisp ("newlisp" "-n" ,(locate-file swank-newlisp-filename load-path))
                    :init swank-newlisp-init
                    :coding-system utf-8-unix))))
    (slime 'newlisp)))


;; SLIME
(setq load-path (cons "~/.emacs.d/utilities/slime" load-path))
(setq inferior-lisp-program "newlisp")
(require 'slime)
(slime-setup)
