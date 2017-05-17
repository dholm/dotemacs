;;; cscope.el --- Set up CScope support. -*- lexical-binding: t; -*-
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
    (helm-cscope-mode t)))


(use-package xcscope
  :if (executable-find "cscope")
  :defer
  :config
  (validate-setq
   ;; Always index recursively.
   cscope-index-recursively t
   ;; Don't display CScope result buffer by default.
   cscope-display-cscope-buffer nil))
(use-package helm-cscope
  :if (executable-find "cscope")
  :defer
  :diminish helm-cscope-mode)


(provide 'utilities/cscope)
;;; cscope.el ends here
