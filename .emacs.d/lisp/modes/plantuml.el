;;; plantuml.el --- Mode support for PlantUML. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--puml-mode-hook ()
  "PlantUML mode hook."
  ;;; (Bindings) ;;;
  (user/bind-key-local :code :auto-complete 'puml-complete-symbol)
  (user/bind-key-local :code :compile 'puml-preview))


(defun user--puml-mode-config ()
  "Initialize PlantUML mode."
  (add-auto-mode 'puml-mode "\\.puml$" "\\.plantuml$")

  ;;; (Hooks) ;;;
  (add-hook 'puml-mode-hook 'user--puml-mode-hook))

(with-executable 'java
  (require-package '(:name puml-mode :after (user--puml-mode-config))))


(provide 'modes/plantuml)
;;; plantuml.el ends here
