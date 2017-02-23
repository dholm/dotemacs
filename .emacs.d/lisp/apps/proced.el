;;; proced.el --- Emacs process manager.
;;; Commentary:
;;; Code:

(unless (eq system-type 'darwin)
  (use-package proced
    :commands proced
    :init
    (user/bind-key-global :apps :processes 'proced)
    :config
    (validate-setq
     ;; Show all processes.
     proced-filter 'all
     ;; Default display mode.
     proced-format 'tiny)

    (add-many-to-list
     'proced-format-alist
     '(min pid tree (args comm))
     '(tiny pid tree user pcpu pmem (args comm)))))


(provide 'apps/proced)
;;; proced.el ends here
