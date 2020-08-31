;;; groovy --- Initializes Groovy mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package groovy-mode
  :config
  (use-package jenkinsfile-mode
    :mode "Jenkinsfile"))


(provide 'modes/groovy)
;;; groovy.el ends here
