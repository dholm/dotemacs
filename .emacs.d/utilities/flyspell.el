;;; flyspell --- spell checking on the fly
;;; Commentary:
;;; Code:

(defun dholm/flyspell-init ()
  "Initialize flyspell."
  ;;; (Faces) ;;;
  )

(after-load 'flyspell
  (dholm/flyspell-init))

(require-package '(:name deferred-flyspell))


(provide 'utilities/flyspell)
;;; flyspell.el ends here
