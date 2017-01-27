;;; proced.el --- Emacs process manager.
;;; Commentary:
;;; Code:

(defun user--proced-config ()
  "Initialize proced."
  (setq-default
   ;; Show all processes.
   proced-filter 'all
   ;; Default display mode.
   proced-format 'tiny)

  (after-load 'proced
    (add-many-to-list
     'proced-format-alist
     '(min pid tree (args comm))
     '(tiny pid tree user pcpu pmem (args comm))))

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :processes 'proced))

(unless (eq system-type 'darwin)
  (user--proced-config))


(provide 'apps/proced)
;;; proced.el ends here
