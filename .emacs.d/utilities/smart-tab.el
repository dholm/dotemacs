;;; smart-tab --- make tab do the right thing
;;; Commentary:
;;; Code:

(require-package '(:name smart-tab :after (dholm/smart-tab-init)))

(defun dholm/smart-tab-init ()
  "Initialize smart tab."
  (global-smart-tab-mode t)
  (diminish 'smart-tab-mode))


(provide 'utilities/smart-tab)
;;; smart-tab.el ends here
