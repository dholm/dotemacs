;;; direx.el --- a simple directory explorer
;;; Commentary:
;;; Code:

(defun user/direx-init ()
  "Initialize direx."
  (setq
   ;; Use the functionality from utilities/project to locate project root
   direx-project:project-root-predicate-functions '(user/project-root-p))

  (after-load 'popwin
    (push '(direx:direx-mode :position left :width 30 :dedicated t)
          popwin:special-display-config))

  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :open-file-context
                        'direx-project:jump-to-project-root-other-window))

(require-package '(:name direx :after (user/direx-init)))


(provide 'utilities/direx)
;;; direx.el ends here
