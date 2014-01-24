;;; init-utilities.el --- initializes utilities
;;; Commentary:
;;; Code:

(load-all-files-from-dir (path-join *user-emacs-lisp-directory* "utilities"))


;;; (Functions) ;;;
(defun user/close-all-buffers ()
  "Close all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(provide 'init-utilities)
;;; init-utilities.el ends here
