;;; mergetool.el --- Emacs as a mergetool
;;; Commentary:
;;; Usage:

;; emacs -Q --load mergetool.el

;;; Code:

(eval-and-compile
  ;; Load Emacs init prologue.
  (load (expand-file-name "prologue.el" user-emacs-directory)))

(eval-when-compile
  (require 'ediff))

(require 'init-vcs)


(progn
  (defun ediff-write-merge-buffer ()
    (let ((file ediff-merge-store-file))
      (set-buffer ediff-buffer-C)
      (write-region (point-min) (point-max) file)
      (message "Merge buffer saved in: %s" file)
      (set-buffer-modified-p nil)
      (sit-for 1)))
  (setq ediff-quit-hook 'kill-emacs
        ediff-quit-merge-hook 'ediff-write-merge-buffer)
  (message "local: %s remote: %s base: %s merged: %s"
           (getenv "LOCAL") (getenv "REMOTE")
           (getenv "BASE") (getenv "MERGED"))
  (ediff-merge-files-with-ancestor (getenv "LOCAL") (getenv "REMOTE")
                                   (getenv "BASE") nil (getenv "MERGED")))


;; Load Emacs init epilogue.
(load (expand-file-name "epilogue.el" user-emacs-directory))
;;; mergetool.el ends here
