;; Gnuplot mode
(require-package '(:name gnuplot-mode :after (dholm/gnuplot-mode-init)))


(defun dholm/gnuplot-mode-init ()
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
  (add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode)))


(provide 'modes/gnuplot)
