;;; scala.el --- initializes Scala modes
;;; Commentary:
;;; Code:

(defun user/scala-mode-hook ()
  "Scala mode hook."
  (user/smartparens-enable)

  ;; Enable YouCompleteMe.
  (user/ycmd-enable)

  (when (el-get-package-is-installed 'ensime)
    (ensime-scala-mode-hook)))


(defun user/scala-mode2-init ()
  "Initialize scala mode 2."
  (add-hook 'scala-mode-hook 'user/scala-mode-hook))


(defun user/scala-mode-init ()
  "Initialize Scala mode."
  (after-load 'smartparens
    (sp-with-modes '(scala-mode)
      (sp-local-pair "'" nil :actions nil)))

  (req-package scala-mode2
    :loader :el-get
    :config (user/scala-mode2-init))

  (with-executable 'sbt
    (req-package ensime)))

(with-executable 'scala
  (user/scala-mode-init))


(provide 'modes/scala)
;;; scala.el ends here
