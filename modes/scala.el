;; Scala

(add-to-list 'load-path "~/.emacs.d/modes/scala-mode2")
(require 'scala-mode2)


;; Ensime
(add-to-list 'load-path "~/.emacs.d/modes/ensime/src/main/elisp")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
