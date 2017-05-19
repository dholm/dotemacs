;;; direx.el --- a simple directory explorer -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package direx
  :defer
  :init
  (user/bind-key-global :basic :open-file-context
                        'direx-project:jump-to-project-root-other-window)
  :config
  (validate-setq
   ;; Use the functionality from utilities/project to locate project root
   direx-project:project-root-predicate-functions
   '((lambda (path)
       (with-project-root project-root path
         (and path (equal (file-truename path) (file-truename project-root)))))))

  (with-eval-after-load 'popwin
    (push '(direx:direx-mode :position left :width 30 :dedicated t)
          popwin:special-display-config)))


(provide 'utilities/direx)
;;; direx.el ends here
