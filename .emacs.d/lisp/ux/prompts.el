;;; prompts.el --- Configure Emacs prompting
;;; Commentary:
;;; Code:

(defun user--prompts-config ()
  "Initialize Emacs prompting."
  (validate-setq
   ;; Always follow links to version controlled files.
   vc-follow-symlinks t)

  ;; Use shorter y/n prompts instead of yes/no.
  (fset 'yes-or-no-p 'y-or-n-p)

  (when (display-graphic-p)
    (validate-setq
     ;; Don't use graphical dialog boxes when prompting.
     use-dialog-box nil))

  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent \"Active processes exist\" query when you quit Emacs."
    (with-feature 'cl-lib
      (cl-flet ((process-list ())) ad-do-it))))

(user--prompts-config)


(provide 'ux/prompts)
;;; prompts.el ends here
