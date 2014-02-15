;;; mergetool.el --- Emacs as a mergetool
;;; Commentary:
;;; Usage:

;; emacs -Q --load mergetool.el

;;; Code:

(eval-and-compile
  ;; Load Emacs init prologue.
  (load (expand-file-name "prologue.el" user-emacs-directory))

  (require 'ediff))


;; Load VCS support.
(require 'init-vcs)


(let ((local (getenv "LOCAL"))
      (remote (getenv "REMOTE"))
      (base (getenv "BASE"))
      (merged (getenv "MERGED")))

  (defun ediff-write-merge-buffer ()
    (let ((file ediff-merge-store-file))
      (set-buffer ediff-buffer-C)
      (write-region (point-min) (point-max) file)
      (message "Merge buffer saved in: %s" file)
      (set-buffer-modified-p nil)
      (sit-for 1)))

  (setq ediff-quit-hook 'kill-emacs
        ediff-quit-merge-hook 'ediff-write-merge-buffer)

  (ediff-merge-files-with-ancestor local remote base nil merged))


;; Load Emacs init epilogue.
(load (expand-file-name "epilogue.el" user-emacs-directory))
;;; mergetool.el ends here
