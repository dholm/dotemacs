;;; gnuplot --- initializes gnuplot mode
;;; Commentary:
;;; Code:

(defun dholm/gnuplot-mode-hook ()
  "Hook for gnuplot mode."
  (setq-default
   ;; Indent using spaces
   indent-tabs-mode nil)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode)
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t))


(defun dholm/gnuplot-mode-init ()
  "Initialize gnuplot mode."
  (add-hook 'gnuplot-mode-hook 'dholm/gnuplot-mode-hook)
  (add-auto-mode 'gnuplot-mode "\\.gp$"))

(require-package '(:name gnuplot-mode :after (dholm/gnuplot-mode-init)))


(provide 'modes/gnuplot)
;;; gnuplot.el ends here
