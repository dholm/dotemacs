;; (Utilities) ;;

;; Close all open buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;; Load all utilities
(require 'utilities/path)
(require 'utilities/compile)
(require 'utilities/xcscope)
(require 'utilities/outlookedit)
(require 'utilities/windmove)
(require 'utilities/uniquify)
(require 'utilities/tabbar)
(require 'utilities/saveplace)
(require 'utilities/savehist)
