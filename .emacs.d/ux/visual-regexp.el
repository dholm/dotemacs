;;; visual-regexp --- visualize regular expressions
;;; Commentary:
;;; Code:

(defun dholm/visual-regexp-init ()
  ;;; (Bindings) ;;;
  (global-set-key [remap query-replace-regexp] 'vr/query-replace)
  (global-set-key [remap replace-regexp] 'vr/replace))

(require-package '(:name visual-regexp :after (dholm/visual-regexp-init)))


(provide 'ux/visual-regexp)
;;; visual-regexp.el ends here
