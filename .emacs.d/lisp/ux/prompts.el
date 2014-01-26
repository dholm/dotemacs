;;; prompts.el --- Configure Emacs prompting
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl))

(defun user/prompts-init ()
  "Initialize Emacs prompting."
  (setq-default
   ;; Always follow links to version controlled files.
   vc-follow-symlinks t)

  ;; Use shorter y/n prompts instead of yes/no.
  (fset 'yes-or-no-p 'y-or-n-p)

  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent \"Active processes exist\" query when you quit Emacs."
    (cl-flet ((process-list ())) ad-do-it)))

(user/prompts-init)


(provide 'ux/prompts)
;;; prompts.el ends here
