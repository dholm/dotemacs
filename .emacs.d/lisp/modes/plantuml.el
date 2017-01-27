;;; plantuml.el --- Mode support for PlantUML.
;;; Commentary:
;;; Code:

(defun user/puml-mode-hook ()
  "PlantUML mode hook."
  ;;; (Bindings) ;;;
  (user/bind-key-global :code :auto-complete 'puml-complete-symbol)
  (user/bind-key-global :code :compile 'puml-preview))


(defun user/puml-mode-init ()
  "Initialize PlantUML mode."
  (add-auto-mode 'puml-mode "\\.puml$" "\\.plantuml$")

  ;;; (Hooks) ;;;
  (add-hook 'puml-mode-hook 'user/puml-mode-hook))

(with-executable 'java
  (req-package puml-mode
    :loader :el-get
    :config (user/puml-mode-init)))


(provide 'modes/plantuml)
;;; plantuml.el ends here
