;;; proced.el --- Emacs process manager.
;;; Commentary:
;;; Code:

(defun user--proced-config ()
  "Initialize proced."
  (validate-setq
   ;; Show all processes.
   proced-filter 'all
   ;; Default display mode.
   proced-format 'tiny)

  (add-many-to-list
   'proced-format-alist
   '(min pid tree (args comm))
   '(tiny pid tree user pcpu pmem (args comm)))

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :processes 'proced))

(unless (eq system-type 'darwin)
  (use-package proced
    :defer t
    :config (user--proced-config)))


(provide 'apps/proced)
;;; proced.el ends here
