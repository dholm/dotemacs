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

(with-executable 'scala
  (use-package scala-mode
    :defer
    :init
    (add-hook 'scala-mode-hook 'user--scala-mode-hook)
    :config
    (after-load 'smartparens
      (sp-with-modes '(scala-mode)
        (sp-local-pair "'" nil :actions nil)))
    (with-executable 'sbt
      (use-package ensime))))


(provide 'modes/scala)
;;; scala.el ends here
