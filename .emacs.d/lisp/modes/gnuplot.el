;;; gnuplot --- initializes gnuplot mode
;;; Commentary:
;;; Code:

(defun user/gnuplot-mode-hook ()
  "Hook for gnuplot mode.")


(defun user/gnuplot-mode-init ()
  "Initialize gnuplot mode."
  (add-hook 'gnuplot-mode-hook 'user/gnuplot-mode-hook)
  (add-auto-mode 'gnuplot-mode "\\.gp$"))

(with-executable 'gnuplot
  (require-package '(:name gnuplot-mode :after (user/gnuplot-mode-init))))


(provide 'modes/gnuplot)
;;; gnuplot.el ends here
