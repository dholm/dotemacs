;;; profiler.el --- Configure Emacs profiler
;;; Commentary:
;;; Code:

(defun user/profiler-init ()
  "Initialize Emacs profiler."
  (define-key user/utilities-map (kbd "p p") 'profiler-start)
  (define-key user/utilities-map (kbd "p P") 'profiler-stop)
  (define-key user/utilities-map (kbd "p r") 'profiler-report)
  (define-key user/utilities-map (kbd "p s") 'profiler-report-write))

(when (fboundp 'profiler-start)
  (user/profiler-init))


(provide 'utilities/abbrev)
;;; profiler.el ends here
