;;; flyspell --- spell checking on the fly
;;; Commentary:
;;; Code:

(defun user/flyspell-init ()
  "Initialize flyspell."
  ;;; (Faces) ;;;
  )

(after-load 'flyspell
  (user/flyspell-init))

(require-package '(:name deferred-flyspell))


(provide 'utilities/flyspell)
;;; flyspell.el ends here
