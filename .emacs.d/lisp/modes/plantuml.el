;;; plantuml.el --- Mode support for PlantUML. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-plantuml-jar-path*
  (path-join *user-cache-directory* "plantuml.jar")
  "Path to user's PlantUML jar file.")


(use-package plantuml-mode
  :defer
  :mode (("\\.puml$" . plantuml-mode))
  :bind-wrap
  (:map plantuml-mode-map
        ((:key :code :complete) . plantuml-complete-symbol)
        ((:key :code :compile) . plantuml-preview))
  :init
  (let ((plantuml-url "https://sourceforge.net/projects/plantuml/files/latest/download?source=files"))
    (when (not (file-exists-p *user-plantuml-jar-path*))
      (url-copy-file plantuml-url *user-plantuml-jar-path*)))
  :config
  (validate-setq
   plantuml-jar-path *user-plantuml-jar-path*)

  (with-eval-after-load 'org-src
    (add-to-list
     ;; Enable PlantUML editing in org-mode code blocks.
     'org-src-lang-modes '("plantuml" . plantuml))))


(provide 'modes/plantuml)
;;; plantuml.el ends here
