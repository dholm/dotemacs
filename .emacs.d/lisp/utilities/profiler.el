;;; profiler.el --- Configure Emacs profiler
;;; Commentary:
;;; Code:

(defun user/profiler-init ()
  "Initialize Emacs profiler."
  (user/bind-key-global :emacs :profiler-start 'profiler-start)
  (user/bind-key-global :emacs :profiler-stop 'profiler-stop)
  (user/bind-key-global :emacs :profiler-report 'profiler-report)
  (user/bind-key-global :emacs :profiler-save 'profiler-report-write))

(when (fboundp 'profiler-start)
  (user/profiler-init))


(provide 'utilities/abbrev)
;;; profiler.el ends here
