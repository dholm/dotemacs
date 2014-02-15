;;; ediff.el --- Configuration for ediff
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'ediff))


(defun user/ediff-mode-hook ()
  "Ediff mode hook."
  (setq
   ;; Don't wrap long lines.
   truncate-lines t))


(defun user/ediff-mergetool ()
  "Launch ediff as mergetool."
  (defun ediff-write-merge-buffer ()
    "Write merge buffer to file."
    (let ((file ediff-merge-store-file))
      (set-buffer ediff-buffer-C)
      (write-region (point-min) (point-max) file)
      (message "Merge buffer saved in: %s" file)
      (set-buffer-modified-p nil)
      (sit-for 1)))

  (setq
   ediff-quit-hook 'kill-emacs
   ediff-quit-merge-hook 'ediff-write-merge-buffer)

  (let ((local (pop command-line-args-left))
        (remote (pop command-line-args-left))
        (base (pop command-line-args-left))
        (merged (pop command-line-args-left)))
    (ediff-merge-files-with-ancestor local remote base nil merged)))


(defun user/ediff-difftool ()
  "Launch ediff as difftool."
  (let ((local (pop command-line-args-left))
        (remote (pop command-line-args-left)))
    (ediff local remote)))


(defun user/ediff-init ()
  "Initialize ediff."
  (setq-default
   ;; Ignore changes in whitespace.
   ediff-diff-options "-w"
   ediff-ignore-similar-regions t)

  ;; Go to first difference on start.
  (add-hook 'ediff-startup-hook 'ediff-next-difference))

(after-load 'ediff
  (user/ediff-init))


(provide 'utilities/ediff)
;;; ediff.el ends here
