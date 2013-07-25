;;; init-utilities --- initializes utilities
;;; Commentary:
;;; Code:

;; Load all utilities
(require 'utilities/auto-complete)
(require 'utilities/bookmark+)
(require 'utilities/browse-kill-ring)
(require 'utilities/cedet)
(require 'utilities/compile)
(require 'utilities/deft)
(require 'utilities/dtrt-indent)
(require 'utilities/ecb)
(require 'utilities/elim)
(require 'utilities/flycheck)
(require 'utilities/flymake)
(require 'utilities/flyspell)
(require 'utilities/helm)
(require 'utilities/hide-lines)
(require 'utilities/ibuffer)
(when (display-graphic-p)
  (require 'utilities/minimap))
(require 'utilities/outlookedit)
(require 'utilities/packed)
(require 'utilities/perspective)
(require 'utilities/popup)
(require 'utilities/smart-tab)
(require 'utilities/tramp)
(require 'utilities/undo-tree)
(require 'utilities/url)
(require 'utilities/w3m)
(require 'utilities/xcscope)
(require 'utilities/yasnippet)


;;; (Functions) ;;;
(defun close-all-buffers ()
  "Close all open buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(provide 'init-utilities)
;;; init-utilities.el ends here
