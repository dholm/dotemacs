;;; cscope.el --- Set up CScope support.
;;; Commentary:
;;; Code:

(defun user/cscope-create/update ()
  "Create or update CScope database at current project root."
  (interactive)
  (with-executable 'cscope
    (with-project-root project-root nil
      (cond
       ((require 'cedet-cscope nil :noerror)
        (cedet-cscope-create/update-database project-root))
       ((require 'xcscope nil :noerror)
        (cscope-index-files project-root))))))


(defun user/cscope-enable ()
  "Activate CScope in current major mode."
  (with-feature 'xcscope
    (with-project-root project-root nil
      (cscope-set-initial-directory project-root))
    (cscope-setup))

  (with-feature 'helm-cscope
    (after-load 'diminish
      (diminish 'helm-cscope-mode))
    (helm-cscope-mode t)))


(defun user--xcscope-config ()
  "Initialize xcscope."
  (validate-setq
   ;; Always index recursively.
   cscope-index-recursively t
   ;; Don't display CScope result buffer by default.
   cscope-display-cscope-buffer nil))


(defun user--cscope-config ()
  "Initialize CScope support."
  ;;; (Packages) ;;;
  (use-package xcscope
    :defer t
    :config (user--xcscope-config))
  (use-package helm-cscope
    :defer t))

(with-executable 'cscope
  (user--cscope-config))


(provide 'utilities/cscope)
;;; cscope.el ends here
