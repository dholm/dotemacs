;;; scala.el --- initializes Scala modes
;;; Commentary:
;;; Code:

(defun user--scala-mode-hook ()
  "Scala mode hook."
  (user/smartparens-enable)

  ;; Enable YouCompleteMe.
  (user/ycmd-enable)

  (when (feature-p 'ensime)
    (ensime-scala-mode-hook)))


(defun user--scala-mode2-config ()
  "Initialize scala mode 2."
  (add-hook 'scala-mode-hook 'user--scala-mode-hook))


(defun user--scala-mode-config ()
  "Initialize Scala mode."
  (after-load 'smartparens
    (sp-with-modes '(scala-mode)
      (sp-local-pair "'" nil :actions nil)))

  (require-package '(:name scala-mode2 :after (user--scala-mode2-config)))

  (with-executable 'sbt
    (use-package ensime
      :defer t)))

(with-executable 'scala
  (user--scala-mode-config))


(provide 'modes/scala)
;;; scala.el ends here
