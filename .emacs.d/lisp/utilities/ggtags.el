;;; ggtags.el --- GNU/Global tags support
;;; Commentary:
;;; Code:

(defun user/ggtags-mode-hook ()
  "Mode hook for ggtags."
  (after-load 'diminish
    (diminish 'ggtags-mode)))


(defun user/ggtags-init ()
  "Initialize ggtags."
  (add-hook 'ggtags-mode-hook 'user/ggtags-mode-hook))


(when *has-global*
  (require-package '(:name ggtags :after (user/ggtags-init))))


(provide 'utilities/ggtags)
;;; ggtags.el ends here
