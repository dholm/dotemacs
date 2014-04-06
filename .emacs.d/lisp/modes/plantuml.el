;;; plantuml.el --- Mode support for PlantUML.
;;; Commentary:
;;; Code:

(defun user/plantuml-mode-init ()
  "Initialize PlantUML mode.")

(with-executable 'java
  (require-package '(:name plantuml-mode :after (user/plantuml-mode-init))))


(provide 'modes/plantuml)
;;; plantuml.el ends here
