;;; basic.el --- Emacs as a basic editor
;;; Commentary:
;;; Usage:

;; emacs -Q --load ~/.emacs.d/kind/basic.el

;;; Code:

(eval-and-compile
  ;; Load Emacs init prologue.
  (load (expand-file-name "prologue.el" user-emacs-directory)))


;; Load Emacs init epilogue.
(load (expand-file-name "epilogue.el" user-emacs-directory))
;;; basic.el ends here
