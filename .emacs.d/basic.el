;;; basic.el --- Emacs as a basic editor
;;; Commentary:
;;; Usage:

;; emacs -Q --load basic.el [switch] [params]

;;; Code:

(eval-and-compile
  ;; Load Emacs init prologue.
  (load (expand-file-name "prologue.el" user-emacs-directory)))


(defun user/emacs-vcstool-handler (switch)
  "Trampoline for VCS-tool SWITCH."
  (load (path-join user-emacs-directory "lisp" "utilities" "ediff.el"))
  (cond
   ((equal switch "--difftool")
    (user/ediff-difftool))
   ((equal switch "--mergetool")
    (user/ediff-mergetool))))


(defun user--emacs-basic-config ()
  "Initialize basic Emacs."
  ;; Register switch hook handler.
  (add-command-switch
   'user/emacs-vcstool-handler "--difftool" "--mergetool"))

(user--emacs-basic-config)


;; Load Emacs init epilogue.
(load (expand-file-name "epilogue.el" user-emacs-directory))
;;; basic.el ends here
