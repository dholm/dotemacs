;;; ibuffer --- improved buffer management
;;; Commentary:
;;; Code:

(defun user/ibuffer-hook ()
  "Hook for improved buffer."
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))


(defun user/ibuffer-init ()
  "Initialize improved buffer."
  (setq-default ibuffer-filter-group-name-face 'font-lock-doc-face)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (add-hook 'ibuffer-hook 'user/ibuffer-hook))

(user/ibuffer-init)

(require-package '(:name ibuffer-vc
                         :type github
                         :pkgname "purcell/ibuffer-vc"))


(provide 'utilities/ibuffer)
;;; ibuffer.el ends here
