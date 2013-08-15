;;; init-utilities.el --- initializes utilities
;;; Commentary:
;;; Code:

(require 'utilities/browse-kill-ring)
(require 'utilities/cedet)
(require 'utilities/compile)
(require 'utilities/ctable)
(require 'utilities/ecb)
(require 'utilities/epc)
(require 'utilities/find-file-in-project)
(require 'utilities/flycheck)
(require 'utilities/flymake)
(require 'utilities/flyspell)
(require 'utilities/google-this)
(require 'utilities/helm)
(require 'utilities/ibuffer)
(require 'utilities/ido)
(require 'utilities/outlookedit)
(require 'utilities/perspective)
(require 'utilities/tramp)
(require 'utilities/url)
(require 'utilities/wc-mode)


;;; (Functions) ;;;
(defun user/close-all-buffers ()
  "Close all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(provide 'init-utilities)
;;; init-utilities.el ends here
