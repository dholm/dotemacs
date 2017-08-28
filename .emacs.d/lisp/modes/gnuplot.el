;;; gnuplot --- initializes gnuplot mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gnuplot-mode
  :if (executable-find "gnuplot")
  :defer
  :mode "\\.gp$")


(provide 'modes/gnuplot)
;;; gnuplot.el ends here
