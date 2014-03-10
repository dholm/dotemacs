;;; scala.el --- initializes Scala modes
;;; Commentary:
;;; Code:

(defun user/scala-mode-hook ()
  "Scala mode hook."
  (when (el-get-package-is-installed 'ensime)
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

  (with-executable 'sbt
    (require-package '(:name ensime))))

(with-executable 'scala
  (user/scala-mode-init))


(provide 'modes/scala)
;;; scala.el ends here
