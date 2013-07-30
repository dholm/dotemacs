;;; scala --- initializes Scala modes
;;; Commentary:
;;; Code:

(defconst *user-has-sbt* (executable-find "sbt"))

(defun dholm/scala-mode-hook ()
  "Scala mode hook."
  (when *user-has-sbt* (ensime-scala-mode-hook)))


(defun dholm/scala-mode2-init ()
  "Initialize scala mode 2."
  (add-hook 'scala-mode-hook 'dholm/scala-mode-hook))

(require-package '(:name scala-mode2 :after (dholm/scala-mode2-init)))
(when *user-has-sbt*
  (require-package '(:name ensime)))


(provide 'modes/scala)
;;; scala.el ends here
