;;; profiler.el --- Configure Emacs profiler
;;; Commentary:
;;; Code:

(defun user/profiler-report-mode-hook ()
  "Profiler report mode hook."
  ;;; (Bindings) ;;;
  (user/bind-key-local :emacs :save 'profiler-report-write-profile)
  (user/bind-key-local :emacs :save-as 'profiler-report-write-profile))


(defun user/profiler-init ()
  "Initialize Emacs profiler."
  (setq-default
   ;; The maximum number distinct of call-stacks to save.
   profiler-log-size 100000
   ;; Maximum call-stack depth to record.
   profiler-max-stack-depth 32)

  (add-hook 'profiler-report-mode-hook 'user/profiler-report-mode-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :emacs :profiler-start 'profiler-start)
  (user/bind-key-global :emacs :profiler-stop 'profiler-stop)
  (user/bind-key-global :emacs :profiler-report 'profiler-report))

(when (fboundp 'profiler-start)
  (user/profiler-init))


(provide 'utilities/profiler)
;;; profiler.el ends here
