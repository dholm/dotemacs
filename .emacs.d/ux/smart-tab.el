;;; smart-tab.el --- make tab do the right thing
;;; Commentary:
;;; Code:

(defun user/smart-tab-init ()
  "Initialize smart tab."
  (global-smart-tab-mode t)
  (after-load 'diminish
    (diminish 'smart-tab-mode)))

(require-package '(:name smart-tab :after (user/smart-tab-init)))


(provide 'ux/smart-tab)
;;; smart-tab.el ends here
