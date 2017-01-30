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

  (add-hook 'ibuffer-hook 'user--ibuffer-hook)

  ;;; (Bindings) ;;;
  (global-set-key [remap list-buffers] 'ibuffer)

  ;;; (Packages) ;;;
  (use-package ibuffer-vc
    :defer t))

(user--ibuffer-config)


(provide 'utilities/ibuffer)
;;; ibuffer.el ends here
