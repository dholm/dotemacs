;;; plantuml.el --- Mode support for PlantUML. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-plantuml-jar-path*
  (path-join *user-cache-directory* "plantuml.jar")
  "Path to user's PlantUML jar file.")


(defun user--plantuml-mode-hook ()
  "PlantUML mode hook."
  ;;; (Bindings) ;;;
  (user/bind-key-local :code :auto-complete 'puml-complete-symbol)
  (user/bind-key-local :code :compile 'puml-preview))


(use-package plantuml-mode
  :defer
  :mode (("\\.puml$" . plantuml-mode))
  :init
  (add-hook 'plantuml-mode-hook 'user--plantuml-mode-hook)

  (let ((plantuml-url "http://sourceforge.net/projects/plantuml/files/plantuml.jar/download"))
    (when (not (file-exists-p *user-plantuml-jar-path*))
      (url-copy-file plantuml-url *user-plantuml-jar-path*)))
  :config
  (validate-setq
   plantuml-jar-path *user-plantuml-jar-path*))


(provide 'modes/plantuml)
;;; plantuml.el ends here
