;;; scala.el --- initializes Scala modes
;;; Commentary:
;;; Code:

(defun user/scala-mode-hook ()
  "Scala mode hook."
  (when *has-sbt*
    (ensime-scala-mode-hook))

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.scala" "*.java")))


(defun user/scala-mode2-init ()
  "Initialize scala mode 2."
  (add-hook 'scala-mode-hook 'user/scala-mode-hook))


(defun user/scala-mode-init ()
  "Initialize Scala mode."
  (require-package '(:name scala-mode2 :after (user/scala-mode2-init)))

  (when *has-sbt*
    (require-package '(:name ensime))))

(when *has-scala*
  (user/scala-mode-init))


(provide 'modes/scala)
;;; scala.el ends here
