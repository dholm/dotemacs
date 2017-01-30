;;; gnuplot --- initializes gnuplot mode
;;; Commentary:
;;; Code:

(defun user--gnuplot-mode-hook ()
  "Hook for gnuplot mode.")


(defun user--gnuplot-mode-config ()
  "Initialize gnuplot mode."
  (add-hook 'gnuplot-mode-hook 'user--gnuplot-mode-hook)
  (add-auto-mode 'gnuplot-mode "\\.gp$"))

(with-executable 'gnuplot
  (use-package gnuplot-mode
    :ensure t
    :config (user--gnuplot-mode-config)))


(provide 'modes/gnuplot)
;;; gnuplot.el ends here
