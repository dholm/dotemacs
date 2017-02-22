;;; ibuffer --- improved buffer management
;;; Commentary:
;;; Code:

(defun user--ibuffer-hook ()
  "Hook for improved buffer."
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(use-package ibuffer
  :bind* (([remap list-buffers] . ibuffer))
  :init
  (add-hook 'ibuffer-hook 'user--ibuffer-hook)
  :config
  (validate-setq
   ibuffer-filter-group-name-face 'font-lock-doc-face)

  (use-package ibuffer-vc
  :defer t))


(provide 'utilities/ibuffer)
;;; ibuffer.el ends here
