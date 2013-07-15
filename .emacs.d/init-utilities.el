;;; (Utilities) ;;;

;; Load all utilities
(require 'utilities/browse-kill-ring)
(require 'utilities/undo-tree)
(when (display-graphic-p)
  (require 'utilities/minimap))
(require 'utilities/smart-tab)
(require 'utilities/w3m)
(require 'utilities/bookmark+)
(require 'utilities/yasnippet)
(require 'utilities/dtrt-indent)
(require 'utilities/helm)
(require 'utilities/flymake)
(require 'utilities/flyspell)
(require 'utilities/flycheck)
(require 'utilities/auto-complete)
(require 'utilities/cedet)
(require 'utilities/ecb)
(require 'utilities/ibuffer)
(require 'utilities/compile)
(require 'utilities/xcscope)
(require 'utilities/outlookedit)
(require 'utilities/elim)
(require 'utilities/deft)
(require 'utilities/perspective)
(require 'utilities/packed)


;;; (Functions) ;;;
(defun close-all-buffers ()
  "Close all open buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(provide 'init-utilities)
