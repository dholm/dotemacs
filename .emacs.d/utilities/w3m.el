;;; w3m --- w3m integration
;;; Commentary:
;;; Code:

(defconst *has-w3m* (executable-find "w3m"))


(defun user/w3m-init ()
  "Initialize w3m."
  (setq-default
   w3m-use-cookies t
   w3m-coding-system 'utf-8
   w3m-file-coding-system 'utf-8
   w3m-file-name-coding-system 'utf-8
   w3m-input-coding-system 'utf-8
   w3m-output-coding-system 'utf-8
   w3m-terminal-coding-system 'utf-8
   browse-url-browser-function 'w3m-browse-url)

  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (define-key user/utilities-map (kbd "b") 'browse-url-at-point))


(when *has-w3m*
  (require-package '(:name emacs-w3m :after (user/w3m-init))))


(provide 'utilities/w3m)
;;; w3m.el ends here
