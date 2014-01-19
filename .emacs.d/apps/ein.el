;;; ein.el --- Emacs IPython Notebook
;;; Commentary:
;;; Code:

(defun user/ein-init ()
  "Initialize EIN."
  (setq-default
   ;; Enable auto completion.
   ein:use-auto-complete t)

  (define-key user/utilities-map (kbd "p") 'ein:notebooklist-open))


(when *has-ipython*
  (require-package '(:name ein :after (user/ein-init))))


(provide 'apps/ein)
;;; ein.el ends here
