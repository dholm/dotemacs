;;; ibuffer --- improved buffer management
;;; Commentary:
;;; Code:

(defun user--ibuffer-hook ()
  "Hook for improved buffer."
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))


(defun user--ibuffer-config ()
  "Initialize improved buffer."
  (setq-default
   ibuffer-filter-group-name-face 'font-lock-doc-face)

  (add-hook 'ibuffer-hook 'user--ibuffer-hook))

(use-package ibuffer
  :bind* (([remap list-buffers] . ibuffer))
  :config (user--ibuffer-config))

(use-package ibuffer-vc
  :after ibuffer
  :defer t)

(provide 'utilities/ibuffer)
;;; ibuffer.el ends here
