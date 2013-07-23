;;; ace-jump-mode --- quick buffer navigation
;;; Commentary:
;;; Code:

(defun dholm/ace-jump-mode-init ()
  (global-set-key (kbd "C-c n SPC") 'ace-jump-mode))

(require-package '(:name ace-jump-mode :after (dholm/ace-jump-mode-init)))


(provide 'ux/ace-jump-mode)
;;; ace-jump-mode.el ends here
