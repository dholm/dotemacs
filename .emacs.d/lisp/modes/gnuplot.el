;;; gnuplot --- initializes gnuplot mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-executable 'gnuplot
  (use-package gnuplot-mode
    :defer
    :init
    (add-auto-mode 'gnuplot-mode "\\.gp$")))


(provide 'modes/gnuplot)
;;; gnuplot.el ends here
