;;; mmm --- initializes "multiple major modes"
;;; Commentary:
;;; Code:

(defcustom dholm/after-mmm-install-hook ()
  "Ḧook that runs after mmm has been installed"
  :group 'packaging
  :type 'hook)


(defun dholm/run-after-mmm-install-hook ()
  (require 'mmm-auto)
  (dolist (func dholm/after-mmm-install-hook)
    (ignore-errors (funcall func))))


(require-package '(:name mmm-mode
			 :after (dholm/run-after-mmm-install-hook)))

(setq mmm-global-mode 'maybe
      mmm-submode-decoration-level 2)


(provide 'modes/mmm)
;;; mmm.el ends here
