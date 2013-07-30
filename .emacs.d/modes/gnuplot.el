;;; gnuplot --- initializes gnuplot mode
;;; Commentary:
;;; Code:

(defun dholm/gnuplot-mode-hook ()
  "Hook for gnuplot mode.")


(defun dholm/gnuplot-mode-init ()
  "Initialize gnuplot mode."
  (add-hook 'gnuplot-mode-hook 'dholm/gnuplot-mode-hook)
  (add-auto-mode 'gnuplot-mode "\\.gp$"))

(require-package '(:name gnuplot-mode :after (dholm/gnuplot-mode-init)))


(provide 'modes/gnuplot)
;;; gnuplot.el ends here
