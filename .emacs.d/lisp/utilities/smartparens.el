;;; smartparens.el --- Set up smartparens.
;;; Commentary:
;;; Code:

(defun user/smartparens-enable ()
  "Enable smartparens in current mode."
  (show-smartparens-mode t)
  (smartparens-mode t))


(require-package '(:name smartparens))


(provide 'utilities/smartparens)
;;; smartparens.el ends here
