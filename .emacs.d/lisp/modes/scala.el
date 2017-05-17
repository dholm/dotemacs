;;; scala.el --- initializes Scala modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--scala-mode-hook ()
  "Scala mode hook."
  (user/smartparens-enable)

  (when (feature-p 'ensime)
    (ensime-scala-mode-hook)))

(use-package scala-mode
  :if (executable-find "scala")
  :defer
  :init
  (add-hook 'scala-mode-hook 'user--scala-mode-hook)
  :config
  (after-load 'smartparens
    (sp-with-modes '(scala-mode)
      (sp-local-pair "'" nil :actions nil)))
  (use-package ensime
    :if (executable-find "sbt")))


(provide 'modes/scala)
;;; scala.el ends here
