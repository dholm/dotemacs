;;; ediff.el --- Configuration for ediff
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl))


(defun user--ediff-mode-hook ()
  "Ediff mode hook."
  (setq
   ;; Don't wrap long lines.
   truncate-lines t))


(defun user--ediff-startup-hook ()
  "Ediff startup hook."
  (setq-default
   ;; Split window differently depending on frame width.
   ediff-split-window-function (if (> (frame-width) (* 2 80))
                                   'split-window-horizontally
                                 'split-window-vertically))

  ;; Go to the first difference on startup.
  (ediff-next-difference))


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

  (setq-default
   ediff-quit-hook 'kill-emacs
   ediff-quit-merge-hook 'ediff-write-merge-buffer)

  (let ((local (pop command-line-args-left))
        (remote (pop command-line-args-left))
        (base (pop command-line-args-left))
        (merged (pop command-line-args-left)))
    (lexical-let (;; Show only conflicts.
                  (ediff-show-clashes-only t))
      (ediff-merge-files-with-ancestor local remote base nil merged))))


(defun user/ediff-difftool ()
  "Launch ediff as difftool."
  (let ((local (pop command-line-args-left))
        (remote (pop command-line-args-left)))
    (ediff local remote)))


(defun user--ediff-config ()
  "Initialize ediff."
  (setq-default
   ;; Don't create a separate frame for ediff.
   ediff-window-setup-function 'ediff-setup-windows-plain
   ;; Ignore changes in whitespace.
   ediff-diff-options "-w"
   ediff-ignore-similar-regions t)

  ;; Go to first difference on start.
  (add-hook 'ediff-startup-hook 'user--ediff-startup-hook)

  ;;; (Packages) ;;;
  (use-package ztree
    :defer t))

(user--ediff-config)


(provide 'utilities/ediff)
;;; ediff.el ends here
