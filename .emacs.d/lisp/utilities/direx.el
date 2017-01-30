;;; direx.el --- a simple directory explorer
;;; Commentary:
;;; Code:

(defun user--direx-config ()
  "Initialize direx."
  (setq
   ;; Use the functionality from utilities/project to locate project root
   direx-project:project-root-predicate-functions
   '((lambda (path)
       (with-project-root project-root path
         (and path (equal (file-truename path) (file-truename project-root)))))))

  (after-load 'popwin
    (push '(direx:direx-mode :position left :width 30 :dedicated t)
          popwin:special-display-config))

  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :open-file-context
                        'direx-project:jump-to-project-root-other-window))

(use-package direx
  :ensure t
  :config (user--direx-config))


(provide 'utilities/direx)
;;; direx.el ends here
