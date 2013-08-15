;;; init-utilities.el --- initializes utilities
;;; Commentary:
;;; Code:

;; Load all utilities
(require 'utilities/bookmark+)
(require 'utilities/browse-kill-ring)
(require 'utilities/calfw)
(require 'utilities/cedet)
(require 'utilities/compile)
(require 'utilities/ctable)
(require 'utilities/deft)
(require 'utilities/ecb)
(require 'utilities/elim)
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
(require 'utilities/w3m)
(require 'utilities/wc-mode)


;;; (Functions) ;;;
(defun user/close-all-buffers ()
  "Close all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(provide 'init-utilities)
;;; init-utilities.el ends here
