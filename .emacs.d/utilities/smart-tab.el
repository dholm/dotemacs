;;; smart-tab --- make tab do the right thing
;;; Commentary:
;;; Code:

(defun dholm/smart-tab-init ()
  "Initialize smart tab."
  (global-smart-tab-mode t)
  (after-load 'diminish
    (diminish 'smart-tab-mode)))

(require-package '(:name smart-tab :after (dholm/smart-tab-init)))


(provide 'utilities/smart-tab)
;;; smart-tab.el ends here
