;;; ein.el --- Emacs IPython Notebook
;;; Commentary:
;;; Code:

(defun user/ein-init ()
  "Initialize EIN."
  (setq-default
   ;; Enable auto completion.
   ein:use-auto-complete t
   ;; Store request package data in cache directory.
   request-storage-directory (path-join *user-cache-directory* "request"))

  (define-key user/utilities-map (kbd "n") 'ein:notebooklist-open))


(when *has-ipython*
  (require-package '(:name ein :after (user/ein-init))))


(provide 'apps/ein)
;;; ein.el ends here
