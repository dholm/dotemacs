;;; ibuffer --- improved buffer management
;;; Commentary:
;;; Code:

(require-package '(:name ibuffer-vc))


(defun dholm/ibuffer-hook ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'dholm/ibuffer-hook)

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(global-set-key (kbd "C-x C-b") 'ibuffer)


(provide 'utilities/ibuffer)
;;; ibuffer.el ends here
