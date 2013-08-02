;;; visual-regexp --- visualize regular expressions
;;; Commentary:
;;; Code:

(defun user/visual-regexp-init ()
  ;;; (Bindings) ;;;
  (global-set-key [remap query-replace-regexp] 'vr/query-replace)
  (global-set-key [remap replace-regexp] 'vr/replace))

(require-package '(:name visual-regexp :after (user/visual-regexp-init)))


(provide 'ux/visual-regexp)
;;; visual-regexp.el ends here
